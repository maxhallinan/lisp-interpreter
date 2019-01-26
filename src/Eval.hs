{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Eval where

import Control.Exception (SomeException, fromException, throw, try)
import Data.Traversable (traverse)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import qualified LispVal as L
import qualified Parser as Parser
import qualified Prim as Prim

-- | Define the default evaluation environment
basicEnv :: Map.Map String L.LispVal
basicEnv = Map.fromList $ Prim.primEnv
  <> [ ("read", L.Fun . L.IFunc . Prim.unop $ readFn)
     , ("parse", L.Fun . L.IFunc . Prim.unop $ parseFn)
     , ("eval", L.Fun . L.IFunc . Prim.unop $ eval)
     , ("show", L.Fun . L.IFunc . Prim.unop $ (return . L.Str . show))
     ]

-- | Evaluate a Lisp file
evalFile :: String -> String -> IO ()
evalFile filename file = result >>= (print . show . fst)
  where eval    = fileToEval filename file 
        result  = runASTInEnv basicEnv eval

evalStr :: String -> IO L.LispVal
evalStr str = evalStrInEnv basicEnv str >>= (return . fst)

evalStrInEnv :: L.EnvCtx -> String -> IO (L.LispVal, L.EnvCtx)
evalStrInEnv env str = result
  where eval    = strToEval str
        result  = runASTInEnv env eval

safeExec :: IO a -> IO (Either String a)
safeExec m = do
  result <- try m
  case result of
    Left (exception :: SomeException) ->
      case fromException exception of
        Just (lispException :: L.LispException) -> 
          return $ Left (show lispException)
        Nothing -> 
          return $ Left (show exception)
    Right x -> 
      return $ Right x

-- | Parse a Lisp string into a Lisp AST
fileToEval :: String -> String -> L.Eval L.LispVal
fileToEval filename file = 
  either 
    (throw . L.ParseError . show) 
    evalBody 
    (Parser.readSexprFile filename file)

strToEval :: String -> L.Eval L.LispVal
strToEval str = 
  either 
    (throw . L.ParseError . show) 
    evalBody
    (Parser.readSexpr str)

-- | Test s-expression parser
runParseTest :: String -> String
runParseTest input = either show show $ Parser.readSexpr input

-- | Evaluate a Lisp program in the provided evaluation environment
runASTInEnv :: L.EnvCtx -> L.Eval a -> IO (a, L.EnvCtx)
runASTInEnv env evalM = S.runStateT (L.unEval evalM) env

-- | Parse a Lisp string
parseFn :: L.LispVal -> L.Eval L.LispVal
parseFn (L.Str s) = either (throw . L.ParseError . show) return $ Parser.readSexpr s
parseFn x         = throw $ L.TypeMismatch "string" x

-- | Parse and evaluate a Lisp string
readFn :: L.LispVal -> L.Eval L.LispVal
readFn (L.Str s)  = either (throw . L.ParseError . show) eval $ Parser.readSexpr s
readFn x          = throw $ L.TypeMismatch "string" x

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
  env     <- S.get
  -- pairs is a list where even items (0-based indexing) are symbols (variables)
  -- odd items are expresions bound to the variables
  -- collect/validate the variables
  symbols <- traverse validateSymbol $ getEven pairs
  -- collect/evaluate the expressions
  values  <- traverse eval $ getOdd pairs
  -- bind evaluated expressions to variables 
  let env' = Map.fromList (zipWith (\a b -> (extractVar a, b)) symbols values) <> env
  -- evaluate the expression with the updated environment
  -- M.liftM (S.withStateT $ const env') (evalBody expr)
  L.Eval $ S.withStateT (const env') (L.unEval $ evalBody expr)
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
  env <- S.get 
  S.put $ Map.insert (extractVar sym) val env
  return val
-- evaluate the special form `lambda`
eval (L.List [L.Symbol "lambda", L.List params, expr]) = do
  env <- S.get
  return $ L.Lambda (L.IFunc $ applyLambda expr params) env
eval (L.List (L.Symbol "lambda":_)) = throw $ L.BadSpecialForm "lambda"
-- evaluate the special form `delay`
eval (L.List [L.Symbol "delay", expr]) = do
  env <- S.get
  return $ L.Lambda (L.IFunc thunk) env
  where thunk (L.Nil : [])  = applyLambda expr [] []
        thunk args          = throw $ L.NumArgs 1 args
eval (L.List [L.Symbol "car", L.List [L.Symbol "quote", L.List (x:_)]]) =
  return $ x
eval (L.List [L.Symbol "car", arg@(L.List (x:_))]) =
  case x of
    L.Symbol _  -> do val <- eval arg
                      eval $ L.List [L.Symbol "car", val] 
    _           -> return $ x
eval (L.List [L.Symbol "cdr", L.List [L.Symbol "quote", L.List (_:xs)]]) = 
  return $ L.List xs
eval (L.List [L.Symbol "cdr", arg@(L.List (x:xs))]) = do
  case x of
    L.Symbol _  -> do val <- eval arg
                      eval $ L.List [L.Symbol "cdr", val]
    _           -> return $ L.List xs

-- evaluate function application
eval (L.List ((:) x xs)) = do
  fn  <- eval x
  x   <- traverse eval xs
  case fn of
    (L.Fun (L.IFunc f)) -> f x
    (L.Lambda (L.IFunc f) env) -> do
      S.put env
      f x
    _ -> throw $ L.NotFunction fn

-- | Takes a function body, a list of parameter symbols, and a list of argument 
-- expressions.
-- Evaluates the arguments, binds the argument values to the parameters, and 
-- evaluates the function body within that environment.
applyLambda :: L.LispVal -> [L.LispVal] -> [L.LispVal] -> L.Eval L.LispVal
applyLambda expr params args = do
  env <- S.get
  -- evaluate the argument expressions
  args' <- traverse eval args
  -- bind the evaluated argument values to the parameter names
  -- combine with existing environment
  let env' = Map.fromList (zipWith (\a b -> (extractVar a, b)) params args') <> env
  -- evaluate the function body with updated environment
  S.put env' 
  eval expr

-- | Evaluate a body expression
evalBody :: L.LispVal -> L.Eval L.LispVal
evalBody (L.List [L.List ((:) (L.Symbol "define") [L.Symbol var, expr]), rest]) = do
  val <- eval expr
  env <- S.get
  let env' = Map.insert var val env
  S.put env'
  eval rest
evalBody (L.List ((:) (L.List ((:) (L.Symbol "define") [L.Symbol var, expr])) rest)) = do
  val <- eval expr
  env <- S.get
  let env' = Map.insert var val env
  S.put env'
  evalBody (L.List rest)
evalBody x = eval x

-- | Get the evenly indexed items in a list 
getEven :: [a] -> [a]
getEven [] = []
getEven (x:xs) = x : getOdd xs

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
  env <- S.get
  case Map.lookup s env of
    Just x -> return x
    Nothing -> throw $ L.UnboundVar s
