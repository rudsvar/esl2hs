-- | A module for helper functions.
module Util where

import Data.Char (toUpper)

-- | Replace the first character of a string with its
-- uppercase equivalent if possible.
upperFirst :: String -> String
upperFirst []       = error "upperFirst called on empty string"
upperFirst (c : cs) = toUpper c : cs

-- | A typeclass for types that can be pretty-printed.
class Pretty a where
  -- | A function that turns the argument into a pretty string.
  pretty :: a -> String
