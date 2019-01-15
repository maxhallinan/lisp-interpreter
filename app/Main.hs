module Main where
import qualified System.Environment as Env

import Lib
import Control.Monad
import qualified Parser as Parser
import qualified Eval as Eval
import qualified System.IO as Sys

flushStr :: String -> IO ()
flushStr str = putStr str >> Sys.hFlush Sys.stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint expr = Eval.evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
main = do 
  args <- Env.getArgs 
  case length args of
    0 -> runRepl  
    1 -> evalAndPrint $ args !! 0
    otherwise -> putStrLn "This program only takes 0 or 1 argument"
