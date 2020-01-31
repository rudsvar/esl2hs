module Main where

import Lib (esl2hs)
import System.Environment

main :: IO ()
main = do
  [f]      <- getArgs
  contents <- readFile f
  putStr $ esl2hs f contents
