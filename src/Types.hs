module Types where

import Util

data Data = Data
  { dataName :: String
  , typeAliases :: [Alias]
  , constructors :: [Constructor]
  }

data TypeExpr
  = TypeName String
  | Star TypeExpr
  | Plus TypeExpr
  | Optional TypeExpr
  | Product TypeExpr TypeExpr
  deriving (Eq, Ord)

instance Show TypeExpr where
  show (  TypeName name) = upperFirst name
  show (  Star     e   ) = "[" ++ show e ++ "]"
  show (  Plus     e   ) = "(NonEmpty " ++ show e ++ ")"
  show (  Optional e   ) = "(Maybe " ++ show e ++ ")"
  show p@(Product a b  ) = "(" ++ show a ++ ", " ++ show b ++ ")"

data Constructor = Constructor
  { constructorName :: String
  , args :: [TypeExpr]
  , output :: TypeExpr
  }

instance Show Constructor where
  show (Constructor name args _) =
    upperFirst name ++ " " ++ unwords (map show args)

data Alias = Alias String TypeExpr

instance Show Alias where
  show (Alias name e) = "type " ++ name ++ " = " ++ show e

data ESLStmt = A Alias | C Constructor

instance Show ESLStmt where
  show (A a) = show a
  show (C c) = show c

