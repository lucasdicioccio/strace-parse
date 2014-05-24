{-# LANGUAGE NoMonomorphismRestriction #-}

{- TODO:
 - * flag truncated structs/argument lists
 - * parse return values
 - * parse leading timestamps (-t, -tt, and -ttt options)
 - * durations in return values (-T option)
 - * `+++` syntax when a process did not handle a signal
 - * syscall-specific interpretations (maybe should be another step)
 - * other weird syntaxes like the funcall + && + == expressions in
 -      `wait4(-1, [{WIFEXITED(s) && WEXITSTATUS(s) == 0}], WNOHANG, NULL) = 19159`
 -}

module Text.Parsec.Strace where

import Text.Parsec
import Text.Parsec
import Control.Applicative ((<$>), (<*>), (*>), (<*), liftA2)
import Control.Monad (void)
import Data.Maybe (catMaybes)

data Syscall = Syscall (Maybe PID) String [Term] ReturnValue
  deriving Show

data Sighandle = Sighandle (Maybe PID) String
  deriving Show

data Term = Decimal String
  | Hexadecimal String
  | Octal String
  | Product (String, String)
  | QuotedString String
  | TruncatedString String
  | MapStruct [(Term, Term)]
  | KwArgs [(Term, Term)]
  | ListStruct [Term]
  | Array [Term]
  | OrFlags [String]
  | ListFlags (Maybe Bool) [String]
  | Comment String
  | Funcall String [Term]
  | Literal String
  | Ellipsis
  | Null
  deriving Show

data Unsupported = Unsupported String
  deriving Show

type ReturnValue = String
type PID = String

data Either3 a b c = Left3 a | Mid3 b | Right3 c
  deriving Show

left3s :: [Either3 a b c] -> [a]
left3s xs = foldl f [] xs
  where f as (Left3 a) = a:as
        f as _         = as

type Strace = [Either3 Unsupported Sighandle Syscall]

type Parser a = Parsec String () a

strace :: Parser Strace 
strace = many line
  where line = try (Mid3 <$> sighandle)
               <|> try (Right3 <$> syscall)
               <|> (Left3 <$> unsupported)

unsupported = Unsupported <$> manyTill anyChar newline
               
sighandle = Sighandle <$> pid <*> body <?> "sighandle"
  where pid = optionMaybe (posDecimal <* spaces)
        body = do
            (string "--- ")
            ret <- anyChar `manyTill` (try $ string " ---")
            newline
            return ret

syscall = Syscall <$> pid <*> callName <*> arglist <*> rest <?> "syscall"
  where rest = many1 space *> char '=' *> many1 space >> manyTill anyChar newline
        pid = optionMaybe (posDecimal <* spaces)
        callName = many1 (alphaNum <|> oneOf "_")

arglist = do
  char '('
  r <- arg `sepBy` string ", "
  char ')'
  return r

arg :: Parser Term
arg = try (fmap Comment comment)
  <|> (try ellipsis >> return Ellipsis)
  <|> try listStruct
  <|> try listStruct2
  <|> try mapStruct
  <|> try kwArgs
  <|> try array
  <|> try (fmap TruncatedString truncatedString)
  <|> try (fmap QuotedString quotedString)
  <|> try (fmap Product decimalProduct)
  <|> try (fmap Decimal decimal)
  <|> try (fmap Octal octal)
  <|> try (fmap Hexadecimal hexadecimal)
  <|> try null'
  <|> try orFlags
  <|> try listFlags
  <|> try funcall
  <|> try literal
  <?> "arg"

funcall = Funcall <$> lit <*> arglist
literal = Literal <$> lit

lit :: Parser String
lit = many1 (alphaNum <|> oneOf "-_")

null' :: Parser Term
null' = string "null" >> return Null

array :: Parser Term
array = (do
  char '['
  xs <- arg `sepBy` string ", "
  char ']'
  return $ Array xs) <?> "array"

mapStruct :: Parser Term
mapStruct = (do
  char '{'
  kvs <- keyValue `sepBy1` string ", "
  char '}'
  return $ MapStruct (catMaybes kvs)) <?> "mapStruct"

kwArgs :: Parser Term
kwArgs = (do
  kvs <- keyValue `sepBy1` string ", "
  return $ KwArgs (catMaybes kvs)) <?> "mapStruct"

listStruct :: Parser Term
listStruct = (do
  char '{'
  kvs <- arg `sepBy1` string ", "
  char '}'
  return $ ListStruct kvs) <?> "listStruct"

orFlags :: Parser Term
orFlags = OrFlags <$> (flag `sepBy1` (void (char '|') <|> void (string " or "))) <?> "orFlags"

listFlags :: Parser Term
listFlags = (do
  neg <- optionMaybe (char '~')
  let n = (const True) <$> neg
  char '['
  xs <- flag `sepBy1` spaces
  char ']'
  return $ ListFlags n xs) <?> "listFlags"

listStruct2 :: Parser Term
listStruct2 = (do
  char '{'
  xs <- arg `sepBy1` spaces
  char '}'
  return $ ListStruct xs) <?> "listSt"

flag :: Parser String
flag = many1 (oneOf "_ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")
  <|> octal
  <?> "flag"

keyValue :: Parser (Maybe (Term, Term))
keyValue = Just <$> try (liftA2 (,) (key <* char '=') value)
  <|> fmap (const Nothing) ellipsis
  <?> "key-value pair"
  where key = try funcall <|> literal
        value = arg

decimalProduct :: Parser (String, String)
decimalProduct = liftA2 (,) (posDecimal <* char '*') posDecimal

decimal :: Parser String
decimal = negDecimal <|> posDecimal <|> zero <?> "decimal number"

zero = many1 (char '0') <* notFollowedBy (oneOf "xabcdef")

negDecimal :: Parser String
negDecimal = liftA2 (:) (char '-') posDecimal <?> "negative decimal"

posDecimal :: Parser String
posDecimal = (do
  x <- oneOf "123456789"
  xs <- many digit
  notFollowedBy (oneOf "xabcdef") 
  return (x:xs)) <?> "positive decimal"

octal :: Parser String
octal = char '0' >> many1 (oneOf "01234567") <?> "octal number"

hexadecimal :: Parser String
hexadecimal = optional (string "0x") >> many1 hexDigit' <* notFollowedBy alphaNum <?> "hexadecimal number"

hexDigit' :: Parser Char
hexDigit' = oneOf "1234567890abcdef"

comment :: Parser String
comment = string "/*" >> manyTill anyChar (string "*/") <?> "comment"

ellipsis :: Parser ()
ellipsis = void (string "...") <?> "ellipsis"

truncatedString :: Parser String
truncatedString = quotedString <* ellipsis <?> "truncated string"

quotedPair :: Parser String
quotedPair       = do char '\\'
                      r <- noneOf "\r\n"
                      return ['\\',r]
                   <?> "quoted pair"

quotedString :: Parser String
quotedString    = do char '"'
                     r <- many qcont
                     char '"'
                     return ("\"" ++ concat r ++ "\"")
                  <?> "quoted string"
  where
  qtext = noneOf "\\\"\r\n"
  qcont = (many1 qtext) <|> quotedPair
