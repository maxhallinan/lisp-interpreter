module Prim (PrimEnv, primEnv, unaryOp) where

import Control.Exception (throw)
import qualified LispVal as L

type PrimEnv = [(String, L.LispVal)]

primEnv :: PrimEnv
primEnv = []

type Unary = L.LispVal -> L.Eval L.LispVal

unaryOp :: Unary -> [L.LispVal] -> L.Eval L.LispVal
unaryOp op [x] = op x
unaryOp _ args = throw $ L.NumArgs 1 args
