module Eval where

import Data.Traversable (mapM)
import Control.Monad.Except (catchError, throwError)
import qualified Parser as Parser

data LispError 
  = NumArgs Integer [Parser.LispVal]
  | TypeMismatch String Parser.LispVal
  | ParseError Parser.ParseError
  | BadSpecialForm String Parser.LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  deriving Eq

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected ++ " arguments; found " ++ (unwords $ map show found)
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ "; found" ++ (show found)
  show (ParseError err) = "Parse error: " ++ show err
  show (BadSpecialForm message lispVal) = message ++ ": " ++ (show lispVal)
  show (NotFunction message lispVal) = message ++ ": " ++ (show lispVal)
  show (UnboundVar message varName) = message ++ ": " ++ varName
  show (Default message) = message

type ThrowsError = Either LispError

-- trapError :: Monad m => m a -> m a
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError Parser.LispVal
readExpr input = case Parser.parse "" input of
  Left err -> throwError $ ParseError err
  Right lispVal -> return lispVal

eval :: Parser.LispVal -> ThrowsError Parser.LispVal
eval val@(Parser.Atom _)   = val
eval val@(Parser.String _) = val
eval val@(Parser.Number _) = val
eval val@(Parser.Bool _)   = val
eval (Parser.ProperList [Parser.Atom "quote", val]) = val
eval (Parser.ProperList (Parser.Atom f : args)) = apply f $ map eval args

apply :: String -> [Parser.LispVal] -> ThrowsError Parser.LispVal
apply f args = 
  maybe 
    (throwError $ NotFunction "Unrecognized primitive function args" f) 
    ($ args) 
    (lookup f primitives)

primitives :: [(String, [Parser.LispVal] -> ThrowsError Parser.LispVal)]
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
              , ("symbol->string", to1Arity symbolToString)
              , ("string->symbol", to1Arity stringToSymbol)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [Parser.LispVal] -> ThrowsError Parser.LispVal
numericBinop op []    = throwError $ NumArgs 2 []
numericBinop op x@[_] = throwError $ NumArgs 2 x
numericBinop op args  = mapM unpackNum args >>= return . Parser.Number . foldl1 op

unpackNum :: Parser.LispVal -> ThrowsError Integer
unpackNum (Parser.Number n) = return n
unpackNum (Parser.String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ Parser.String n
                              else return $ fst $ parsed !! 0
unpackNum (Parser.ProperList [n]) = unpackNum n
unpackNum x = throwError $ TypeMismatch "number" x

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

-- What should these functions return if given a value that isn't a string/atom?

symbolToString :: Parser.LispVal -> Parser.LispVal
symbolToString  (Parser.Atom s) = Parser.String s

stringToSymbol :: Parser.LispVal -> Parser.LispVal
stringToSymbol  (Parser.String s) = Parser.Atom s
