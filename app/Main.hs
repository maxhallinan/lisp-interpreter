module Main where
import qualified System.Environment as Env

import Lib
import qualified Parser as Parser
import qualified Eval as Eval

main :: IO ()
main = Env.getArgs >>= print . show . Eval.eval . Eval.readExpr . head
