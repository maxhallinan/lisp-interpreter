module Parser where

import Control.Applicative ((<|>), empty)
import Control.Monad.Combinators (between)
import qualified Data.List as List
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import qualified Text.Megaparsec.Expr as Expr
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as Err
import Data.Void (Void)

type Parser = Mega.Parsec Void String

type ParseError = Err.ParseError Char Void

data LispVal
  = Atom String
  | Number Integer
  | String String
  | Bool Bool
  | ProperList [LispVal]
  | ImproperList [LispVal] LispVal
  deriving (Eq)

instance Show LispVal where
  show (Atom a) = a
  show (Number i) = show i
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (ProperList l) = "(" ++ (showLispList l) ++ ")"
  show (ImproperList l x) = "(" ++ (showLispList l) ++ show x ++ ")"

showLispList :: [LispVal] -> String
showLispList = unwords . map show

readExpr :: String -> LispVal
readExpr input = case Parser.parse "" input of
  Left err -> String $ "Error: " ++ show err
  Right lispVal -> lispVal

parse :: String -> String -> Either ParseError LispVal
parse filename input = Mega.parse lispSyntax filename input

lispSyntax :: Parser LispVal
lispSyntax = between space Mega.eof lispVal

lispVal :: Parser LispVal
lispVal = lexeme $ bool <|> atom <|> number <|> string <|> list

space :: Parser ()
space = Lex.space Char.space1 lineComment blockComment
  where
    lineComment :: Parser ()
    lineComment = Lex.skipLineComment ";"

    blockComment :: Parser ()
    blockComment = Lex.skipBlockComment ";" "\n"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: String -> Parser String
symbol = Lex.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

escaped :: Char -> Parser Char
escaped x = Char.char '\\' >> Char.char x

quoteChar :: Parser Char
quoteChar = escaped '"'

newlineChar :: Parser Char
newlineChar = escaped 'n' >> return '\n'

escapeChar :: Parser Char
escapeChar = escaped '\\'

returnChar :: Parser Char
returnChar = escaped 'r' >> return '\r'

tabChar :: Parser Char
tabChar = escaped 't' >> return '\t'

escapedChar :: Parser Char
escapedChar = 
      Mega.try quoteChar 
  <|> Mega.try newlineChar 
  <|> Mega.try escapeChar 
  <|> Mega.try returnChar 
  <|> Mega.try tabChar

atom :: Parser LispVal
atom = do
  first <- Char.letterChar <|> Char.symbolChar
  rest <- Mega.many (Char.noneOf ") ") 
  let atom = first : rest
  return $ Atom atom

bool :: Parser LispVal
bool = do
  Char.char '#'
  bool <- toBool <$> (Char.char 't' <|> Char.char 'f')
  return $ Bool bool
  where
    toBool 'f' = False
    toBool 't' = True

number :: Parser LispVal
number = do
  number <- Mega.some Char.digitChar
  return $ Number (read number)

string :: Parser LispVal
string = do
  Char.char '"'
  stringValue <- Mega.many (escapedChar <|> Char.noneOf "\"")
  Char.char '"'
  return $ String stringValue

list :: Parser LispVal
list = do
  symbol "("
  x <- Mega.try improperList <|> properList
  symbol ")"
  return x

properList :: Parser LispVal
properList = do
  terms <- Mega.many lispVal
  return $ ProperList terms

improperList :: Parser LispVal
improperList = do
  head <- Mega.manyTill lispVal (symbol ".")
  tail <- lispVal
  return $ ImproperList head tail
