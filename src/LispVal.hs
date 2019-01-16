{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispVal where

import qualified Data.Map as Map
import qualified Data.Typeable as T
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import Data.Text as T

data LispVal 
  = Atom T.Text 
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool 
  deriving (T.Typeable)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: R.ReaderT EnvCtx IO a }
  deriving  ( Monad
            , Functor
            , Applicative
            , R.MonadReader EnvCtx
            , R.MonadIO
            )

instance Show LispVal where
  show = T.unpack . showLispVal

showLispVal :: LispVal -> T.Text
showLispVal x = 
  case x of
    (Atom atom) -> atom
    (String str) -> T.concat ["\"", str, "\""]
    (Number num) -> T.pack $ show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil -> "Nil"
    (List xs) -> T.concat ["(", T.unwords $ showLispVal <$> xs, ")"]
    (Fun _) -> "<internal function>"
    (Lambda _ _) -> "<lambda function>"
