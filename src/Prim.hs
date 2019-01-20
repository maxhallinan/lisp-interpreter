module Prim 
  ( PrimEnv
  , binop
  , binopFold
  , primEnv
  , unop
  ) where

import Control.Exception (throw)
import Control.Monad (foldM)
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import qualified LispVal as L

type PrimEnv  = [(String, L.LispVal)]
type Unary    = L.LispVal -> L.Eval L.LispVal
type Binary   = L.LispVal -> L.LispVal -> L.Eval L.LispVal

primEnv :: PrimEnv
primEnv = 
  [ ("+", mkFn $ binopFold (numOp (+)))
  , ("*", mkFn $ binopFold (numOp (*)))
  , ("-", mkFn $ binopFold (numOp (-))) 
  , ("<", mkFn $ binop (numCmp (<))) 
  , (">", mkFn $ binop (numCmp (>))) 
  , ("<=", mkFn $ binop (numCmp (<=))) 
  , (">=", mkFn $ binop (numCmp (>=))) 
  , ("==", mkFn $ binop (numCmp (==))) 
  -- , ("++", ) 
  -- , ("even?", ) 
  -- , ("odd?", ) 
  -- , ("pos?", ) 
  -- , ("neg?", ) 
  -- , ("eq?", ) 
  -- , ("bl-eq?", ) 
  -- , ("and", ) 
  -- , ("or", ) 
  -- , ("cons", ) 
  -- , ("cdr", ) 
  -- , ("car", ) 
  -- , ("file?", ) 
  -- , ("slurp?", ) 
  ]

mkFn :: ([L.LispVal] -> L.Eval L.LispVal) -> L.LispVal
mkFn = L.Fun . L.IFunc

unop :: Unary -> [L.LispVal] -> L.Eval L.LispVal
unop op [x] = op x
unop _ args = throw $ L.NumArgs 1 args

binop :: Binary -> [L.LispVal] -> L.Eval L.LispVal
binop op [x, y] = op x y
binop _ args    = throw $ L.NumArgs 2 args

binopFold :: Binary -> [L.LispVal] -> L.Eval L.LispVal
binopFold op args = 
  case args of
    [x, y] -> op x y
    (x:xs) -> foldM op x xs
    otherwise -> throw $ L.NumArgs 2 args

numOp :: (Integer -> Integer -> Integer) -> L.LispVal -> L.LispVal -> L.Eval L.LispVal
numOp op  (L.Int x) (L.Int y) = return $ L.Int (op x y)
numOp _   (L.Int _) y         = throw $ L.TypeMismatch "integer" y
numOp _   x         _         = throw $ L.TypeMismatch "integer" x

numCmp :: (Integer -> Integer -> Bool) -> L.LispVal -> L.LispVal -> L.Eval L.LispVal
numCmp op  (L.Int x) (L.Int y) = return $ L.Bol (op x y)
numCmp _   (L.Int _) y         = throw $ L.TypeMismatch "integer" y
numCmp _   x         _         = throw $ L.TypeMismatch "integer" x
