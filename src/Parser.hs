module Parser where

import Control.Applicative ((<|>))
import Control.Monad (liftM)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Char
import Data.Void (Void)

type Parser = Mega.Parsec Void String

data LispValue
  = LispImproperList [LispValue] LispValue
  | LispProperList [LispValue]
  | LispAtom String
  | LispNumber Integer
  | LispString String
  | LispBool Bool
  deriving (Eq, Show)

-- Datum
-- <datum> := <atom> | <boolean> | <number> | <string> | <list>

lispDatum :: Parser LispValue
lispDatum = lispList <|> lispAtom <|> lispBool <|> lispNumber

lispAtom :: Parser LispValue
lispAtom = do
  first <- Char.letterChar <|> Char.symbolChar <|> Char.asciiChar
  rest <- Mega.many (Char.letterChar <|> Char.digitChar <|> Char.symbolChar)
  let atom = first : rest
  return $ case atom of
              "#t" -> LispBool True
              "#f" -> LispBool False
              _    -> LispAtom atom

lispNumber :: Parser LispValue
lispNumber = do
  number <- Mega.many Char.digitChar
  return $ LispNumber (read number)

lispString :: Parser LispValue
lispString = do
  Char.char '"'
  stringValue <- Mega.many (Char.noneOf "\"")
  Char.char '"'
  return $ LispString stringValue

lispBool :: Parser LispValue
lispBool = do
  Char.char '#'
  bool <- toBool <$> (Char.char 't' <|> Char.char 'f')
  return $ LispBool bool
  where
    toBool 'f' = False
    toBool 't' = True

lispList :: Parser LispValue
lispList = do 
  Char.char '('
  Char.space
  list <- Mega.try lispImproperList <|> lispProperList
  Char.char ')'
  return list

lispProperList :: Parser LispValue
lispProperList = LispProperList <$> sepBySpace lispDatum

lispImproperList :: Parser LispValue
lispImproperList = do
  head <- Mega.endBy lispDatum (Mega.some Char.spaceChar)
  tail <- Char.char '.' >> Char.space >> lispDatum
  return $ LispImproperList head tail

-- Helpers

sepBySpace :: Parser a -> Parser [a]
sepBySpace = (flip Mega.endBy) (Mega.some Char.spaceChar)

beginBySpace :: Parser a -> Parser a
beginBySpace parser = do
  Char.space
  x <- parser
  return x

endBySpace :: Parser a -> Parser a
endBySpace parser = do
  x <- parser
  Char.space
  return x
