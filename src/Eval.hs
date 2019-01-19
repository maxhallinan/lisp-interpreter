{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval where

import Control.Exception (throw)
import Data.Traversable (traverse)
import qualified Control.Monad.Reader as R
import qualified Data.Map as Map
import qualified LispVal as L
import qualified Parser as Parser
import qualified Prim as Prim

-- | Define the default evaluation environment
basicEnv :: Map.Map String L.LispVal
basicEnv = Map.fromList $ Prim.primEnv
  <> [("read", L.Fun . L.IFunc . Prim.unaryOp $ readFn)]

-- | Evaluate a Lisp file
evalFile :: String -> String -> IO ()
evalFile filename file = result >>= print
  where ast     = fileToAST filename file 
        result  = runASTInEnv basicEnv ast

-- | Parse a Lisp string into a Lisp AST
fileToAST :: String -> String -> L.Eval L.LispVal
fileToAST filename file = 
  either 
    (throw . L.PError . show) 
    evalBody 
    (Parser.readSexprFile filename file)

-- | Test s-expression parser
runParseTest :: String -> String
runParseTest input = either show show $ Parser.readSexpr input

-- | Evaluate a Lisp program in the provided evaluation environment
runASTInEnv :: L.EnvCtx -> L.Eval a -> IO a
runASTInEnv env evalM = R.runReaderT (L.unEval evalM) env

readFn :: L.LispVal -> L.Eval L.LispVal
readFn = undefined

-- | Evaluate a Lisp value
eval :: L.LispVal -> L.Eval L.LispVal
-- evaluate a quoted value
eval (L.List [L.Symbol "quote", x]) = return x
-- autoquote data literals
eval (L.Int x)    = return $ L.Int x
eval (L.Str x)    = return $ L.Str x
eval (L.Bol x)    = return $ L.Bol x
-- empty lists arising during evaluation should be represented by the Nil value
eval (L.List [])  = return $ L.Nil
eval L.Nil        = return $ L.Nil
-- stringify a Lisp value
eval (L.List [L.Symbol "write", rest]) = 
  return . L.Str . show $ rest
eval (L.List ((:) (L.Symbol "write") rest)) = 
  return . L.Str . show $ L.List rest
-- return the value bound to the variable
eval s@(L.Symbol _) = getVar s
-- evaluate the special form `if`
eval (L.List [L.Symbol "if", pred, whenTrue, whenFalse]) = do
  condition <- eval pred 
  case condition of
    (L.Bol True) -> eval whenTrue
    (L.Bol False) -> eval whenFalse
    _ -> throw $ L.BadSpecialForm "if" 
-- evaluate the special form `let`
eval (L.List [L.Symbol "let", L.List pairs, expr]) = do
  -- get the environment
  env     <- R.ask
  -- pairs is a list where even items (0-based indexing) are symbols (variables)
  -- odd items are expresions bound to the variables
  -- collect/validate the variables
  symbols <- traverse validateSymbol $ getEven pairs
  -- collect/evaluate the expressions
  values  <- traverse eval $ getOdd pairs
  -- bind evaluated expressions to variables 
  let env' = Map.fromList (zipWith (\a b -> (extractVar a, b)) symbols values) <> env
  -- evaluate the expression with the updated environment
  R.local (const env') $ evalBody expr
-- evaluate the special form `begin`
eval (L.List [L.Symbol "begin", rest]) = evalBody rest
eval (L.List ((:) (L.Symbol "begin") rest)) = evalBody $ L.List rest
-- evaluate the special form `define`
eval (L.List [L.Symbol "define", var, expr]) = do
  -- validate that var is a Symbol
  sym <- validateSymbol var
  -- evaluate expression to lisp value
  val <- eval expr
  -- get the environment
  env <- R.ask 
  -- bind the value to the variable
  let env' = Map.insert (extractVar sym) val env
  -- returns the modified environment 
  R.local (const env') $ return val
-- evaluate the special form `lambda`
eval (L.List [L.Symbol "lambda", L.List params, expr]) = do
  env <- R.ask
  return $ L.Lambda (L.IFunc $ applyLambda expr params) env
eval (L.List (L.Symbol "lambda":_)) = throw $ L.BadSpecialForm "lambda"
-- evaluate the special form `delay`
eval (L.List [L.Symbol "delay", expr]) = do
  env <- R.ask
  return $ L.Lambda (L.IFunc thunk) env
  where thunk (L.Nil : [])  = applyLambda expr [] []
        thunk args          = throw $ L.NumArgs 1 args
-- evaluate function application
eval (L.List ((:) x xs)) = do
  fn  <- eval x
  x   <- traverse eval xs
  case fn of
    (L.Fun (L.IFunc f)) -> f x
    (L.Lambda (L.IFunc f) env) -> R.local (const env) $ f x
    _ -> throw $ L.NotFunction fn

-- | Takes a function body, a list of parameter symbols, and a list of argument 
-- expressions.
-- Evaluates the arguments, binds the argument values to the parameters, and 
-- evaluates the function body within that environment.
applyLambda :: L.LispVal -> [L.LispVal] -> [L.LispVal] -> L.Eval L.LispVal
applyLambda expr params args = do
  env <- R.ask
  -- evaluate the argument expressions
  args' <- traverse eval args
  -- bind the evaluated argument values to the parameter names
  -- combine with existing environment
  let env' = Map.fromList (zipWith (\a b -> (extractVar a, b)) params args') <> env
  -- evaluate the function body with updated environment
  R.local (const env') $ eval expr

-- | Evaluate a body expression
evalBody :: L.LispVal -> L.Eval L.LispVal
evalBody (L.List [L.List ((:) (L.Symbol "define") [L.Symbol var, expr]), rest]) = do
  val <- eval expr
  env <- R.ask
  let env' = Map.insert var val env
  R.local (const env') $ eval rest
evalBody (L.List ((:) (L.List ((:) (L.Symbol "define") [L.Symbol var, expr])) rest)) = do
  val <- eval expr
  env <- R.ask
  let env' = Map.insert var val env
  R.local (const env') $ evalBody (L.List rest)
evalBody x = eval x

-- | Get the evenly indexed items in a list 
getEven :: [a] -> [a]
getEven [] = []
getEven (x:xs) = x : getEven xs

-- | Get the oddly indexed items in a list
getOdd :: [a] -> [a]
getOdd [] = []
getOdd (x:xs) = getEven xs

-- | Throw an error if the Lisp value is not a symbol
validateSymbol :: L.LispVal -> L.Eval L.LispVal
validateSymbol s@(L.Symbol _) = return s
validateSymbol x = throw $ L.TypeMismatch "symbol" x

-- | Get the name of a symbol
extractVar :: L.LispVal -> String
extractVar (L.Symbol s) = s

-- | Get the value bound to a variable
getVar :: L.LispVal -> L.Eval L.LispVal
getVar (L.Symbol s) = do
  env <- R.ask
  case Map.lookup s env of
    Just x -> return x
    Nothing -> throw $ L.UnboundVar s
