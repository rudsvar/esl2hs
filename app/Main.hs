module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [f]      <- getArgs
  contents <- readFile f
  case parse esl f contents of
    Right ast -> putStr (prettyPrint ast)
    Left  err -> putStr $ errorBundlePretty err
