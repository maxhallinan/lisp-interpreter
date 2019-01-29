module Main (main) where

import qualified Test.Tasty as T
import qualified Test.Tasty.Golden as G
import qualified Data.ByteString.Lazy.Char8 as C
import qualified LispVal as L
import qualified Eval as E
import qualified System.Directory as Dir

main :: IO ()
main = T.defaultMain tests

tests :: T.TestTree
tests =
  T.testGroup "special forms"
    [ run "atom-1"    "atom-1.lisp"   "atom-1.txt"
    , run "atom-2"    "atom-2.lisp"   "atom-2.txt"
    , run "car-1"     "car-1.lisp"    "car-1.txt"
    , run "car-1"     "car-1.lisp"    "car-1.txt"
    , run "cdr-1"     "cdr-1.lisp"    "cdr-1.txt"
    , run "cdr-2"     "cdr-2.lisp"    "cdr-2.txt"
    , run "cond-1"    "cond-1.lisp"   "cond-1.txt"
    , run "cond-2"    "cond-2.lisp"   "cond-2.txt"
    , run "cond-3"    "cond-3.lisp"   "cond-3.txt"
    , run "cons-1"    "cons-1.lisp"   "cons-1.txt"
    , run "cons-2"    "cons-2.lisp"   "cons-2.txt"
    , run "eq-1"      "eq-1.lisp"     "eq-1.txt"
    , run "eq-2"      "eq-2.lisp"     "eq-2.txt"
    , run "eq-3"      "eq-3.lisp"     "eq-3.txt"
    , run "eq-4"      "eq-4.lisp"     "eq-4.txt"
    , run "eq-5"      "eq-5.lisp"     "eq-5.txt"
    , run "label-1"   "label-1.lisp"  "label-1.txt"
    , run "label-2"   "label-2.lisp"  "label-2.txt"
    , run "lambda-1"  "lambda-1.lisp" "lambda-1.txt"
    , run "quote-1"   "quote-1.lisp"  "quote-1.txt"
    , run "quote-2"   "quote-2.lisp"  "quote-2.txt"
    ]

run :: String -> String -> String -> T.TestTree
run testName testFile answerFile =
  G.goldenVsString testName answerFile' evaledTest
  where testFile'   = "test-golden/" ++ testFile
        answerFile' = "test-golden/answers/" ++ answerFile
        evaledTest  = evalTest Nothing testFile' >>= (return . C.pack . show)

evalTest :: Maybe String -> String -> IO L.LispVal
evalTest (Just stdlibPath) testPath = do
  stdlib  <- readFileContent stdlibPath
  test    <- readFileContent testPath
  case (stdlib, test) of
    (Just s, Just t) -> do
      (_, env')   <- E.evalFileInEnv E.basicEnv stdlibPath s
      (result, _) <- E.evalStrInEnv env' t
      return result
    _ ->
      return $ L.Nil
evalTest Nothing testPath = do
  test        <- readFileContent testPath
  case test of
    Just t -> do
      (result, _) <- E.evalStrInEnv E.basicEnv t
      return result
    Nothing ->
      return L.Nil

readFileContent :: String -> IO (Maybe String)
readFileContent filePath = do
  isFile <- Dir.doesFileExist filePath
  if isFile
  then Just <$> readFile filePath
  else return Nothing
