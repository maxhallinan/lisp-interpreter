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
  , ("and", mkFn $ binop (bolOp (&&))) 
  , ("or", mkFn $ binop (bolOp (||))) 
  , ("cons", mkFn $ binop cons) 
  , ("cdr", mkFn $ unop cdr) 
  , ("car", mkFn $ unop car) 
  -- , ("odd?", ) 
  -- , ("pos?", ) 
  -- , ("neg?", ) 
  -- , ("even?", ) 
  -- , ("eq?", ) 
  -- , ("bl-eq?", ) 
  -- , ("file?", ) 
  -- , ("slurp?", ) 
  -- , ("++", ) 
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

bolOp :: (Bool -> Bool -> Bool) -> L.LispVal -> L.LispVal -> L.Eval L.LispVal
bolOp op  (L.Bol x) (L.Bol y) = return $ L.Bol (op x y)
bolOp _   (L.Int _) y         = throw $ L.TypeMismatch "boolean" y
bolOp _   x         _         = throw $ L.TypeMismatch "boolean" x

cons :: L.LispVal -> L.LispVal -> L.Eval L.LispVal
cons x  (L.List ys) = return $ L.List (x:ys)
cons x  L.Nil       = return $ L.List [x]
cons _  x           = throw $ L.ExpectedList "cons" x

car :: L.LispVal -> L.Eval L.LispVal
car (L.List [])     = throw $ L.LengthOfList "car" 1
car (L.List (x:_))  = return x
car x               = throw $ L.ExpectedList "cdr" x

cdr :: L.LispVal -> L.Eval L.LispVal
cdr (L.List [])       = throw $ L.LengthOfList "cdr" 1
cdr (L.List [x])      = return L.Nil 
cdr (L.List (_:xs))   = return $ L.List xs
cdr x                 = throw $ L.ExpectedList "cdr" x
