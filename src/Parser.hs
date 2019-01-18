module Parser (readSexpr, readSexprFile) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (between)
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as Err
import Data.Void (Void)
import qualified LispVal as L

type Parser = Mega.Parsec Void String

type ParseError = Err.ParseError Char Void

readSexprFile :: String -> String -> Either ParseError L.LispVal
readSexprFile filename = Mega.parse (contents parseProgram) filename
  where parseProgram = L.List <$> Mega.sepBy sexpr space

readSexpr :: String -> Either ParseError L.LispVal
readSexpr = Mega.parse (contents sexpr) "<stdin>"

contents :: Parser a -> Parser a
contents p = between space Mega.eof p

sexpr :: Parser L.LispVal
sexpr = lexeme $ atom <|> list 

list :: Parser L.LispVal
list = do
  symbol' "("
  xs <- Mega.many sexpr
  symbol' ")"
  case xs of
    [] -> return $ L.Nil
    _ -> return $ L.List xs

atom :: Parser L.LispVal
atom = integer <|> string <|> quote <|> symbol

integer :: Parser L.LispVal
integer = unsignedInteger <|> signedInteger

signedInteger :: Parser L.LispVal
signedInteger = do
  _ <- Char.char '-'
  number <- Mega.some Char.digitChar
  return $ L.Int (negate . read $ number)

unsignedInteger :: Parser L.LispVal
unsignedInteger = do
  number <- Mega.some Char.digitChar
  return $ L.Int (read number)

string :: Parser L.LispVal
string = do
  Char.char '"'
  s <- Mega.many (escapedQuoteChar <|> Char.noneOf "\"")
  Char.char '"'
  return $ L.Str s

quote :: Parser L.LispVal
quote = do
  _ <- Char.char '\''
  x <- sexpr
  return $ L.List [L.Symbol "quote", x]

symbol :: Parser L.LispVal
symbol = do
  sym <-  identifier
          <|> Char.string "+"
          <|> Char.string "-"
          <|> Char.string "..."
  case sym of
    "nil" -> return $ L.Nil
    "true" -> return $ L.Bol True
    "false" -> return $ L.Bol False
    otherwise -> return $ L.Symbol sym
  where 
    identifier = do 
      first <- initial 
      rest  <- Mega.many subsequent
      return $ (first : rest)
    initial     = Char.letterChar <|> Char.oneOf "!$%&*/:<=>?~_^"
    subsequent  = initial <|> Char.digitChar <|> Char.oneOf ".+-"

-- TODO: add support for hash bang (e.g. `#! /bin/sh`) comment
space :: Parser ()
space = Lex.space Char.space1 lineComment blockComment
  where
    lineComment :: Parser ()
    lineComment = Lex.skipLineComment ";"

    blockComment :: Parser ()
    blockComment = Lex.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol' :: String -> Parser String
symbol' = Lex.symbol space

escaped :: Char -> Parser Char
escaped x = Char.char '\\' >> Char.char x

escapedQuoteChar :: Parser Char
escapedQuoteChar = escaped '"'

-- newlineChar :: Parser Char
-- newlineChar = escaped 'n' >> return '\n'

-- escapeChar :: Parser Char
-- escapeChar = escaped '\\'

-- returnChar :: Parser Char
-- returnChar = escaped 'r' >> return '\r'

-- tabChar :: Parser Char
-- tabChar = escaped 't' >> return '\t'

-- escapedChar :: Parser Char
-- escapedChar = 
--       Mega.try quoteChar 
--   <|> Mega.try newlineChar 
--   <|> Mega.try escapeChar 
--   <|> Mega.try returnChar 
--   <|> Mega.try tabChar
