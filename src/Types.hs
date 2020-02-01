-- | Type definitions and `Pretty` implementations.

module Types where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S

import Util

-- | A representation of defined type aliases and data types.
--
--
-- Here is a type alias in ESL.
--
-- > type Bits = Bit*
--
-- And here is the data type Bool.
--
-- > symbol true: -> Bool
-- > symbol false: -> Bool
data Module = Module
  { aliases :: S.Set Alias -- ^ The aliases
  , dataTypes :: S.Set DataType -- ^ The data types
  } deriving (Eq, Ord, Show)

instance Pretty Module where
  pretty m =
    let
      prettyAliases = unlines $ map pretty $ S.toList $ aliases m
      prettyDataTypes =
        intercalate "\n\n" (map pretty $ S.toList $ dataTypes m)
      sep = if null prettyAliases then "" else "\n"
    in prettyAliases ++ sep ++ prettyDataTypes

-- | A representation of a data type.
-- It must have a name and a collection of symbols.
data DataType = DataType
  { typeName :: String -- ^ The name of the data type.
  , symbols :: NonEmpty Symbol -- ^ The symbol definitions of the data type.
  } deriving (Eq, Ord, Show)

instance Pretty DataType where
  pretty (DataType name (c :| cs)) =
    let
      dataEquals   = "data " ++ name ++ "\n  = "
      firstConstr  = pretty c
      otherConstrs = concatMap (("\n  | " ++) . pretty) cs
    in dataEquals ++ firstConstr ++ otherConstrs

-- | A data type representing a type expression.
-- Here is an example representing a list of tuples in ESL.
--
-- > (x, y)*
--
-- One could for instance find it in an alias:
--
-- > type map = (x, y)*
--
-- Or in a symbol declaration:
--
-- > symbol foo: (x, y)* -> Map
data TypeExpr
  = TypeName String -- ^ The name of the type.
  | Star TypeExpr -- ^ Zero or more expressions of the given type.
  | Plus TypeExpr -- ^ One or more expressions of the given type.
  | Optional TypeExpr -- ^ Optionally an expression of the given type.
  | Product [TypeExpr] -- ^ The cartesian product of two types (a tuple).
  deriving (Eq, Ord, Show)

instance Pretty TypeExpr where
  pretty (TypeName name) = upperFirst name
  pretty (Star     e   ) = "[" ++ pretty e ++ "]"
  pretty (Plus     e   ) = "(NonEmpty " ++ pretty e ++ ")"
  pretty (Optional e   ) = "(Maybe " ++ pretty e ++ ")"
  pretty (Product  xs  ) = "(" ++ intercalate ", " (map pretty xs) ++ ")"

-- | A data type representing a symbol declaration.
-- This is equivalent to a constructor in Haskell.
--
-- It could for instance look like this:
--
-- > symbol foo: String, Expr -> Expr
--
-- Which in Haskell would be a constructor with name Foo,
-- the two argument types String and Expr, and output another Expr.
data Symbol = Symbol
  { symbolName :: String
  , args :: [TypeExpr]
  , output :: TypeExpr
  } deriving (Eq, Ord, Show)

instance Pretty Symbol where
  pretty s =
    let
      prettyName = upperFirst (symbolName s)
      prettyArgs = unwords (map pretty (args s))
      sep        = if null prettyArgs then "" else " "
    in prettyName ++ sep ++ prettyArgs

-- | A data type representing a type alias definition.
--
-- Here is an example in ESL:
--
-- > type Bits = Bit*
--
-- And in Haskell this would be very similar:
--
-- > type Bits = [Bit]
data Alias = Alias String TypeExpr deriving (Eq, Ord, Show)

instance Pretty Alias where
  pretty (Alias name e) = "type " ++ name ++ " = " ++ pretty e

-- | An ESL statement can either be an alias or a symbol.
data ESLStmt = A Alias | S Symbol

instance Pretty ESLStmt where
  pretty (A a) = pretty a
  pretty (S s) = pretty s
