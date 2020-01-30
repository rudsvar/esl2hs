module Lib
  ( parse
  , parseTest
  , esl
  , prettyPrint
  , errorBundlePretty
  , groupBy
  ) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Data.Void
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Char (toUpper)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keywords :: [String]
keywords = ["type", "symbol"]

keyword :: String -> Parser ()
keyword = void . symbol

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = label "identifier" $ lexeme $ do
  first <- letterChar
  rest  <- many alphaNumChar
  let name = first : rest
  if name `elem` keywords
    then fail $ show name ++ " is a keyword"
    else return name

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
  { name :: String
  , args :: [TypeExpr]
  , output :: TypeExpr
  }

instance Show Constructor where
  show (Constructor name args _) =
    upperFirst name ++ " " ++ unwords (map show args)

data Alias = Alias String TypeExpr

instance Show Alias where
  show (Alias name e) = "type " ++ name ++ " = " ++ show e

constructor :: Parser Constructor
constructor = do
  keyword "symbol"
  name <- identifier
  keyword ":"
  e1 <- typeExpr `sepBy` symbol ","
  keyword "->"
  e2 <- typeExpr
  return $ Constructor name e1 e2

alias :: Parser Alias
alias = Alias <$> (keyword "type" >> identifier) <*> (keyword "=" >> typeExpr)

typeName :: Parser TypeExpr
typeName = do
  (x : xs) <- identifier
  return $ TypeName (toUpper x : xs)

typeTerm :: Parser TypeExpr
typeTerm = typeName <|> tuple
 where
  tuple = parens $ do
    e1 <- typeExpr
    symbol ","
    e2 <- typeExpr
    return $ Product e1 e2

typeExpr :: Parser TypeExpr
typeExpr = makeExprParser typeTerm operatorTable

operatorTable :: [[Operator Parser TypeExpr]]
operatorTable =
  [ [ Postfix (Star <$ symbol "*")
    , Postfix (Plus <$ symbol "+")
    , Postfix (Optional <$ symbol "?")
    ]
  ]

data ESLStmt = A Alias | C Constructor

instance Show ESLStmt where
  show (A a) = show a
  show (C c) = show c

eslStmt :: Parser ESLStmt
eslStmt = (C <$> constructor) <|> (A <$> alias)

esl :: Parser [ESLStmt]
esl = many eslStmt <* eof

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
    prettyName = upperFirst (name c)
    prettyArgs = unwords (map show (args c))
  in prettyName ++ " " ++ prettyArgs

upperFirst :: String -> String
upperFirst []       = error "upperFirst called on empty string"
upperFirst (c : cs) = toUpper c : cs
