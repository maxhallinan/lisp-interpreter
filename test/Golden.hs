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
  T.testGroup "Golden tests"
    [ run "add" "add.lisp" "add.txt"
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
