module Repl (run) where

import qualified LispVal as L
import qualified Eval as E
import qualified System.Console.Haskeline as H
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State as S

run :: IO ()
run = do
  putStrLn "Welcome."
  H.runInputT H.defaultSettings (loop E.basicEnv)

loop :: L.EnvCtx -> H.InputT IO ()
loop env = do
  mInput <- H.getInputLine "> "
  case mInput of
      Nothing -> H.outputStrLn "Goodbye"
      Just "" -> loop env
      Just ":quit" -> H.outputStrLn "Goodbye"
      Just input -> do
        eResult <- liftIO $ E.safeExec (E.evalStrInEnv env input)
        case eResult of
          Left errMsg -> do
            H.outputStrLn errMsg
            loop env
          Right (result, env') -> do
            H.outputStrLn $ show result
            loop env'
