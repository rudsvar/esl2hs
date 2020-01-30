module Main where

import Lib (parse, esl, errorBundlePretty, prettyPrint)
import System.Environment

main :: IO ()
main = do
  [f]      <- getArgs
  contents <- readFile f
  case parse esl f contents of
    Right ast -> putStr (prettyPrint ast)
    Left  err -> putStr $ errorBundlePretty err
