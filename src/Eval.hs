module Eval where

import Data.Traversable (mapM)
import Control.Monad.Except (catchError, throwError)
import qualified Parser as Parser
import Control.Monad (liftM)

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
  show (NumArgs expected found) = "Expected " ++ show expected ++ " arguments but found " ++ (unwords $ map show found)
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ " but found this value: " ++ (show found)
  show (ParseError err) = "Parse error: " ++ show err
  show (BadSpecialForm message lispVal) = message ++ ": " ++ (show lispVal)
  show (NotFunction message lispVal) = message ++ ": " ++ (show lispVal)
  show (UnboundVar message varName) = message ++ ": " ++ varName
  show (Default message) = message

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> String -> ThrowsError Parser.LispVal
readExpr filename input = 
  case Parser.parse filename input of
    Left err      -> throwError $ ParseError err
    Right lispVal -> return lispVal

evalString :: String -> IO String
evalString expr = 
  return $ extractValue $ trapError (liftM show $ readExpr "" expr >>= eval)

eval :: Parser.LispVal -> ThrowsError Parser.LispVal
eval val@(Parser.Atom _)   = return val
eval val@(Parser.String _) = return val
eval val@(Parser.Number _) = return val
eval val@(Parser.Bool _)   = return val
eval (Parser.ProperList [Parser.Atom "quote", val]) = return val
eval (Parser.ProperList [Parser.Atom "if", predicate, consequent, alternate]) = do
  result <- eval predicate
  case result of
    Parser.Bool False -> eval alternate
    otherwise -> eval consequent
eval (Parser.ProperList (Parser.Atom f : args)) = mapM eval args >>= apply f
eval (Parser.ImproperList (Parser.Atom f : xs) x) = do
  evaledXs <- mapM eval xs
  evaledX <- eval x
  return $ Parser.ImproperList evaledXs evaledX
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
              , ("symbol?", return . to1Arity isSymbol)
              , ("number?", return . to1Arity isNumber)
              , ("string?", return . to1Arity isString)
              , ("bool?", return . to1Arity isBool)
              , ("symbol->string", to1Arity symbolToString)
              , ("string->symbol", to1Arity stringToSymbol)
              , ("=", numBoolBinop (==))
              , ("<", numBoolBinop (<))
              , (">", numBoolBinop (>))
              , ("/=", numBoolBinop (/=))
              , (">=", numBoolBinop (>=))
              , ("<=", numBoolBinop (<=))
              , ("&&", boolBoolBinop (&&))
              , ("||", boolBoolBinop (||))
              , ("string=?", strBoolBinop (==))
              , ("string<?", strBoolBinop (<))
              , ("string>?", strBoolBinop (>))
              , ("string<=?", strBoolBinop (<=))
              , ("string>=?", strBoolBinop (>=))
              , ("car", car)
              , ("cdr", cdr)
              , ("cons", cons)
              ]

car :: [Parser.LispVal] -> ThrowsError Parser.LispVal
car [Parser.ProperList (x : _)] = return x
car [Parser.ImproperList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [Parser.LispVal] -> ThrowsError Parser.LispVal
cdr [Parser.ProperList (_ : xs)] = return $ Parser.ProperList xs
cdr [Parser.ImproperList [_] x] = return x
cdr [Parser.ImproperList (_ : xs) x] = return $ Parser.ImproperList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [Parser.LispVal] -> ThrowsError Parser.LispVal
cons [x, Parser.ProperList []] = return $ Parser.ProperList [x]
cons [x, Parser.ProperList xs] = return $ Parser.ProperList (x : xs)
cons [x1, Parser.ImproperList xs x2] = return $ Parser.ImproperList (x1 : xs) x2
cons [x1, x2] = return $ Parser.ImproperList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [Parser.LispVal] -> ThrowsError Parser.LispVal
eqv [(Parser.Bool x1), (Parser.Bool x2)] = return $ Parser.Bool $ x1 == x2
eqv [(Parser.Number x1), (Parser.Number x2)] = return $ Parser.Bool $ x1 == x2
eqv [(Parser.String x1), (Parser.String x2)] = return $ Parser.Bool $ x1 == x2
eqv [(Parser.Atom x1), (Parser.Atom x2)] = return $ Parser.Bool $ x1 == x2
eqv [(Parser.ImproperList xs1 x1),(Parser.ImproperList xs2 x2)] = 
  eqv [(Parser.ProperList (xs1 ++ [x1])), (Parser.ProperList (xs2 ++ [x2]))]
eqv [(Parser.ProperList xs1), (Parser.ProperList xs2)] = 
  return $ Parser.Bool (isSameLength && isEqvList) 
  where 
    isSameLength = length xs1 == length xs2
    isEqvList = all eqvPair $ zip xs1 xs2
    eqvPair (x1, x2) =
      case eqv [x1, x2] of
        Left _ -> False
        Right (Parser.Bool val) -> val 
eqv [_, _] = return $ Parser.Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

boolBinop :: (Parser.LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [Parser.LispVal] -> ThrowsError Parser.LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Parser.Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [Parser.LispVal] -> ThrowsError Parser.LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [Parser.LispVal] -> ThrowsError Parser.LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [Parser.LispVal] -> ThrowsError Parser.LispVal
boolBoolBinop = boolBinop unpackBool

unpackStr :: Parser.LispVal -> ThrowsError String
unpackStr (Parser.String x) = return x
unpackStr (Parser.Number x) = return $ show x
unpackStr (Parser.Bool x) = return $ show x
unpackStr x = throwError $ TypeMismatch "string" x

unpackBool :: Parser.LispVal -> ThrowsError Bool
unpackBool (Parser.Bool x) = return x
unpackBool x = throwError $ TypeMismatch "boolean" x

numericBinop :: (Integer -> Integer -> Integer) -> [Parser.LispVal] -> ThrowsError Parser.LispVal
numericBinop op []    = throwError $ NumArgs 2 []
numericBinop op x@[_] = throwError $ NumArgs 2 x
numericBinop op args  = unpackArgs args >>= runOp
  where runOp       = return . Parser.Number . foldl1 op
        unpackArgs  = traverse unpackNum

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

symbolToString :: Parser.LispVal -> ThrowsError Parser.LispVal
symbolToString  (Parser.Atom s) = return $ Parser.String s
symbolToString  x = throwError $ TypeMismatch "symbol" x

stringToSymbol :: Parser.LispVal -> ThrowsError Parser.LispVal
stringToSymbol  (Parser.String s) = return $ Parser.Atom s
stringToSymbol  x = throwError $ TypeMismatch "string" x
