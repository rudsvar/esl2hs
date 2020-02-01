-- | A module that re-exports useful functions from "Util" and "ESLParser".
module Lib
  ( esl2hs
  , pretty
  , Module(..)
  ) where

import Util (pretty)
import Parser (parse, esl, errorBundlePretty)
import Types (Module(..))

esl2hs :: String -> String -> String
esl2hs f s = either errorBundlePretty pretty (parse esl f s)
