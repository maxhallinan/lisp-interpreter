module Repl (run) where

import qualified LispVal as L
import qualified Eval as E
import qualified Prim as Prim
import qualified System.Console.Haskeline as H
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State as S

stdlibPath = "lisp/stdlib.lisp"

run :: IO ()
run = do
  stdlibFile <- readFile stdlibPath
  result <- liftIO $ E.safeExec (E.evalFileInEnv E.basicEnv stdlibPath stdlibFile)
  case result of
    Left errMsg ->
      putStrLn errMsg
    Right (_, env') -> do
      putStrLn "Welcome."
      H.runInputT H.defaultSettings (loop env')

loop :: L.EnvCtx -> H.InputT IO ()
loop env = do
  mInput <- H.getInputLine "> "
  case mInput of
      Nothing -> H.outputStrLn "Goodbye"
      Just "" -> loop env
      Just ":quit" -> H.outputStrLn "Goodbye"
      Just input -> do
        result <- liftIO $ E.safeExec (E.evalStrInEnv env input)
        either onError onResult result
  where onError errMsg = do
          H.outputStrLn errMsg
          loop env
        onResult (result, env') = do
          H.outputStrLn $ show result
          loop env'
