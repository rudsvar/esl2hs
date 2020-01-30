module Util where

import Data.Char (toUpper)

upperFirst :: String -> String
upperFirst []       = error "upperFirst called on empty string"
upperFirst (c : cs) = toUpper c : cs
