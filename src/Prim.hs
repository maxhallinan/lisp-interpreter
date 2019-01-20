module Prim
  ( PrimEnv
  , binop
  , binopFold
  , primEnv
  , unop
  ) where

import Control.Exception (throw)
import Control.Monad (foldM)
import Data.Foldable (all, foldl)
import Data.Traversable (traverse)
import qualified System.Directory as Dir
import qualified System.IO as SysIO
import Control.Monad.IO.Class (liftIO)
import qualified LispVal as L

type PrimEnv  = [(String, L.LispVal)]
type Unary    = L.LispVal -> L.Eval L.LispVal
type Binary   = L.LispVal -> L.LispVal -> L.Eval L.LispVal

primEnv :: PrimEnv
primEnv =
  [ ("+", mkFn $ binopFold (numOp (+)))
  , ("*", mkFn $ binopFold (numOp (*)))
  , ("-", mkFn $ binopFold (numOp (-)))
  , ("div", mkFn $ binopFold (numOp (div)))
  , ("<", mkFn $ binop (numCmp (<)))
  , (">", mkFn $ binop (numCmp (>)))
  , ("<=", mkFn $ binop (numCmp (<=)))
  , (">=", mkFn $ binop (numCmp (>=)))
  , ("==", mkFn $ binop (numCmp (==)))
  , ("and", mkFn $ binop (bolOp (&&)))
  , ("or", mkFn $ binop (bolOp (||)))
  , ("cons", mkFn $ binop consOp)
  , ("cdr", mkFn $ unop cdrOp)
  , ("car", mkFn $ unop carOp)
  , ("odd?", mkFn $ unop oddOp)
  , ("even?", mkFn $ unop evenOp)
  , ("pos?", mkFn $ unop (numBol (0 <)))
  , ("neg?", mkFn $ unop (numBol (0 >)))
  , ("++", mkFn $ binop (strOp (++)))
  , ("eq?", mkFn $ binop eqOp)
  , ("file?", mkFn $ unop doesFileExist)
  , ("slurp?", mkFn $ unop slurpOp)
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

numBol :: (Integer -> Bool) -> L.LispVal -> L.Eval L.LispVal
numBol op (L.Int x) = return $ L.Bol (op x)
numBol _  x         = throw $ L.TypeMismatch "integer" x

bolOp :: (Bool -> Bool -> Bool) -> L.LispVal -> L.LispVal -> L.Eval L.LispVal
bolOp op  (L.Bol x) (L.Bol y) = return $ L.Bol (op x y)
bolOp _   (L.Int _) y         = throw $ L.TypeMismatch "boolean" y
bolOp _   x         _         = throw $ L.TypeMismatch "boolean" x

strOp :: (String -> String -> String) -> L.LispVal -> L.LispVal -> L.Eval L.LispVal
strOp op  (L.Str x) (L.Str y) = return $ L.Str (op x y)
strOp _   (L.Str _) y         = throw $ L.TypeMismatch "string" y
strOp _   x         (L.Str _) = throw $ L.TypeMismatch "string" x

consOp :: L.LispVal -> L.LispVal -> L.Eval L.LispVal
consOp x  (L.List ys) = return $ L.List (x:ys)
consOp x  L.Nil       = return $ L.List [x]
consOp _  x           = throw $ L.ExpectedList "cons" x

carOp :: L.LispVal -> L.Eval L.LispVal
carOp (L.List [])     = throw $ L.LengthOfList "car" 1
carOp (L.List (x:_))  = return x
carOp x               = throw $ L.ExpectedList "cdr" x

cdrOp :: L.LispVal -> L.Eval L.LispVal
cdrOp (L.List [])       = throw $ L.LengthOfList "cdr" 1
cdrOp (L.List [x])      = return L.Nil
cdrOp (L.List (_:xs))   = return $ L.List xs
cdrOp x                 = throw $ L.ExpectedList "cdr" x

evenOp :: L.LispVal -> L.Eval L.LispVal
evenOp (L.Int x)  = return $ L.Bol ((mod x 2) == 0)
evenOp x          = throw $ L.TypeMismatch "integer" x

oddOp :: L.LispVal -> L.Eval L.LispVal
oddOp (L.Int x) = return $ L.Bol ((mod x 2) /= 0)
oddOp x         = throw $ L.TypeMismatch "integer" x

eqOp :: L.LispVal -> L.LispVal -> L.Eval L.LispVal
eqOp (L.Symbol x) (L.Symbol y)  = return $ L.Bol (x == y)
eqOp (L.Str x)    (L.Str y)     = return $ L.Bol (x == y)
eqOp (L.Int x)    (L.Int y)     = return $ L.Bol (x == y)
eqOp (L.Bol x)    (L.Bol y)     = return $ L.Bol (x == y)
eqOp L.Nil        L.Nil         = return $ L.Bol True
eqOp _ _                        = return $ L.Bol False

doesFileExist :: L.LispVal -> L.Eval L.LispVal
doesFileExist (L.Symbol s) = doesFileExist $ L.Str s
doesFileExist (L.Str s)    = L.Bol <$> liftIO (Dir.doesFileExist s)
doesFileExist x            = throw $ L.TypeMismatch "string" x

slurpOp :: L.LispVal -> L.Eval L.LispVal
slurpOp (L.Symbol s)  = slurpOp $ L.Str s
slurpOp (L.Str s)     = liftIO $ readTextFile s
slurpOp x             = throw $ L.TypeMismatch "string" x

readTextFile :: String -> IO L.LispVal
readTextFile filename = SysIO.withFile filename SysIO.ReadMode go
  where
    go :: SysIO.Handle -> IO L.LispVal
    go handle = do
      isFile <- SysIO.hIsEOF handle
      if isFile
      then (SysIO.hGetContents handle) >>= (return . L.Str)
      else throw . L.IOError $ concat ["file does not exist: ", filename]
