module Eval where

import qualified Parser as Parser

eval :: Parser.LispVal -> Parser.LispVal
eval val@(Parser.Atom _)   = val
eval val@(Parser.String _) = val
eval val@(Parser.Number _) = val
eval val@(Parser.Bool _)   = val
eval (Parser.ProperList [Parser.Atom "quote", val]) = val
eval (Parser.ProperList (Parser.Atom f : args)) = apply f $ map eval args

apply :: String -> [Parser.LispVal] -> Parser.LispVal
apply f args = maybe (Parser.Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [Parser.LispVal] -> Parser.LispVal)]
primitives =  [ ("+", numericBinop (+))
              , ("-", numericBinop (-))
              , ("*", numericBinop (*))
              , ("/", numericBinop div)
              , ("mod", numericBinop mod)
              , ("quotient", numericBinop quot)
              , ("remainder", numericBinop rem)
              , ("symbol?", to1Arity isSymbol)
              , ("number?", to1Arity isNumber)
              , ("string?", to1Arity isString)
              , ("bool?", to1Arity isBool)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [Parser.LispVal] -> Parser.LispVal
numericBinop op args = Parser.Number $ foldl1 op $ map unpackNum args

unpackNum :: Parser.LispVal -> Integer
unpackNum (Parser.Number n) = n
unpackNum (Parser.String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (Parser.ProperList [n]) = unpackNum n
unpackNum _ = 0

to1Arity :: (a -> b) -> [a] -> b
to1Arity f (x : _) = f x

isSymbol :: Parser.LispVal -> Parser.LispVal
isSymbol (Parser.Atom _) = Parser.Bool True
isSymbol _ = Parser.Bool False

isString :: Parser.LispVal -> Parser.LispVal
isString (Parser.String _) = Parser.Bool True
isString _ = Parser.Bool False

isNumber :: Parser.LispVal -> Parser.LispVal
isNumber (Parser.Number _) = Parser.Bool True
isNumber _ = Parser.Bool False

isBool :: Parser.LispVal -> Parser.LispVal
isBool (Parser.Bool _) = Parser.Bool True
isBool _ = Parser.Bool False
