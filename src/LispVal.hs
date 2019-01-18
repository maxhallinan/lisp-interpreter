{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispVal (LispVal(..), IFunc, EnvCtx, Eval) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.String as S
import qualified Data.Typeable as T
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R

data LispVal 
  = Symbol String 
  | List [LispVal]
  | Int Integer
  | Str String
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bol Bool 
  deriving (T.Typeable)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

type EnvCtx = Map.Map String LispVal

newtype Eval a = Eval { unEval :: R.ReaderT EnvCtx IO a }
  deriving  ( Monad
            , Functor
            , Applicative
            , R.MonadReader EnvCtx
            , R.MonadIO
            )

instance Show LispVal where
  show = showLispVal

showLispVal :: LispVal -> String
showLispVal x = 
  case x of
    (Symbol sym) -> sym
    (Str str) -> concat ["\"", str, "\""]
    (Int int) -> show int
    (Bol True) -> "true"
    (Bol False) -> "false"
    Nil -> "()"
    (List xs) -> concat ["(", S.unwords $ showLispVal <$> xs, ")"]
    (Fun _) -> "<internal function>"
    (Lambda _ _) -> "<lambda function>"
