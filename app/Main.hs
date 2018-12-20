module Main where
import qualified System.Environment as Env

import Lib

main :: IO ()
main = do
  args <- Env.getArgs
  putStrLn ("Hello" ++ args !! 0)
