module Eval where

import qualified Parser as Parser

eval :: Parser.LispVal -> Parser.LispVal
eval val@(Parser.Atom _)   = val
eval val@(Parser.String _) = val
eval val@(Parser.Number _) = val
eval val@(Parser.Bool _)   = val
eval (Parser.ProperList [Parser.Atom "quote", val]) = val
