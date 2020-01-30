module Lib
  ( parse
  , parseTest
  , esl
  , prettyPrint
  , errorBundlePretty
  , groupBy
  ) where

import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Char (toUpper)
import Types
import Util
import Parser

prettyPrint :: [ESLStmt] -> String
prettyPrint stmts =
  let
    (aliases, constructors) = splitStmt stmts
    printedAliases          = map show aliases
    printedData             = map prettyPrintData (groupByOutput constructors)
  in unlines printedAliases ++ "\n" ++ unlines printedData

prettyPrintData :: [Constructor] -> String
prettyPrintData [] = ""
prettyPrintData (c : cs) =
  let
    dataEquals   = "data " ++ show (output c) ++ "\n  = "
    firstConstr  = prettyPrintConstr c ++ "\n"
    otherConstrs = unlines $ map (("  | " ++) . prettyPrintConstr) cs
  in dataEquals ++ firstConstr ++ otherConstrs

prettyPrintConstr :: Constructor -> String
prettyPrintConstr c =
  let
    prettyName = upperFirst (constructorName c)
    prettyArgs = unwords (map show (args c))
  in prettyName ++ " " ++ prettyArgs

splitStmt :: [ESLStmt] -> ([Alias], [Constructor])
splitStmt [] = ([], [])
splitStmt (stmt : stmts) | (xs, ys) <- splitStmt stmts = case stmt of
  A alias  -> (alias : xs, ys)
  C constr -> (xs, constr : ys)

groupByOutput :: [Constructor] -> [[Constructor]]
groupByOutput cs =
  let
    sorted :: [Constructor]
    sorted = sortOn output cs
  in groupBy ((==) `on` output) sorted
