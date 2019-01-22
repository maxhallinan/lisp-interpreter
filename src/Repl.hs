module Repl (mainLoop) where

import qualified LispVal as L
import qualified Eval as E
import qualified System.Console.Haskeline as H
import Control.Monad.IO.Class (liftIO)

type Repl a = H.InputT IO a

run :: IO ()
run = do
  putStrLn "Welcome."
  H.runInputT H.defaultSettings (repl E.basicEnv)

repl :: L.EnvCtx -> Repl ()
repl env = do
  mInput <- H.getInputLine "> "
  case mInput of
    Nothing       -> H.outputStrLn "Goodbye."
    Just ""       -> repl env
    Just ":quit"  -> H.outputStrLn "Goodbye."
    Just input    -> do
      env' <- liftIO $ process env input
      repl env'

process :: L.EnvCtx -> String -> IO L.EnvCtx
process env str = do
  res <- E.safeExec $ E.evalStrInEnv env str
  either handleError return res
  where handleError errorMsg = do
          putStrLn errorMsg
          return env
