module Main where
import qualified System.Environment as Env

import Lib
import Control.Monad
import qualified Parser as Parser
import qualified Eval as Eval

main :: IO ()
main = do 
  args <- Env.getArgs 
  evaled <- return $ liftM show $ Eval.readExpr "fakefile.txt" (head args) >>= Eval.eval
  putStrLn $ Eval.extractValue $ Eval.trapError evaled
