module Main where
import qualified System.Environment as Env

import Lib
import Control.Monad
import qualified Parser as Parser
import qualified Eval as Eval
import qualified System.IO as Sys
import Repl as Repl

main :: IO ()
main = do 
  Repl.run
