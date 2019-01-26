{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispVal
  ( EnvCtx
  , Eval(..)
  , IFunc(..)
  , LispException(..)
  , LispVal(..)
  ) where

import Control.Exception (Exception)
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import qualified Data.Map as M
import Data.String (unwords)
import qualified Data.Typeable as T

data LispVal
  = Symbol String
  | List [LispVal]
  | Int Integer
  | Str String
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bol Bool
  deriving (Eq, T.Typeable)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

instance Eq IFunc where
  (==) _ _ = False

type EnvCtx = M.Map String LispVal

newtype Eval a = Eval { unEval :: S.StateT EnvCtx IO a }
  deriving  ( Monad
            , Functor
            , Applicative
            , S.MonadState EnvCtx
            , S.MonadIO
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
    (List xs) -> concat ["(", unwords $ showLispVal <$> xs, ")"]
    (Fun _) -> "<internal function>"
    (Lambda _ _) -> "<lambda function>"

data LispException
  = NumArgs Integer [LispVal]
  | LengthOfList String Int
  | ExpectedList String LispVal
  | TypeMismatch String LispVal
  | BadSpecialForm String
  | NotFunction LispVal
  | UnboundVar String
  | UnknownError LispVal
  | ParseError String
  | IOError String

instance Exception LispException

instance Show LispException where
  show = showLispException

unwordsLispVal :: [LispVal] -> String
unwordsLispVal xs = unwords $ show <$> xs

showLispException :: LispException -> String
showLispException (NumArgs expected args) =
  concat
    [ "<NumberOfArgumentsError> expected "
    , show expected
    , " arguments but received "
    , unwordsLispVal args
    ]
showLispException (LengthOfList fnName expected) =
  concat
    [ "<LengthOfListError> expected list of length "
    , show expected
    , " in function "
    , fnName
    ]
showLispException (ExpectedList fnName received) =
  concat
    [ "<ExpectedListError> expected a list in function "
    , fnName
    , " but received "
    , show received
    ]
showLispException (TypeMismatch expected received)  =
  concat
    [ "<TypeMismatchError> expected type "
    , expected
    , "but received "
    , show received
    ]
showLispException (BadSpecialForm msg) =
  concat
    [ "<BadSpecialFormError>: "
    , msg
    ]
showLispException (NotFunction received) =
  concat
    [ "<NotFunctionError> expected a function but found "
    , show received
    ]
showLispException (UnboundVar varName) =
  concat
    [ "<UnboundVarError> variable "
    , varName
    , " not found"
    ]
showLispException (UnknownError x) =
  concat
    [ "<UnknownError> evaluation stopped at "
    , show x
    ]
showLispException (ParseError expression) =
  concat
    [ "<ParseError> cannot parse expression "
    , expression
    ]
showLispException (IOError filename) =
  concat
    [ "<IOError> problem reading file "
    , filename
    ]
