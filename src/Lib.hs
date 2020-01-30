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
  show (  TypeName []      ) = error "TypeName can't be empty string"
  show (  TypeName (x : xs)) = toUpper x : xs
  show (  Star     e       ) = "[" ++ show e ++ "]"
  show (  Plus     e       ) = "(NonEmpty " ++ show e ++ ")"
  show (  Optional e       ) = "(Maybe " ++ show e ++ ")"
  show p@(Product a b      ) = "(" ++ showProduct p ++ ")"
   where
    showProduct :: TypeExpr -> String
    showProduct (Product a b) = show a ++ ", " ++ showProduct b
    showProduct other         = show other

data Constructor = Constructor
  { name :: String
  , args :: [TypeExpr]
  , output :: TypeExpr
  }

instance Show Constructor where
  show (Constructor [] _ _) = "ANONYMOUS"
  show (Constructor (x : xs) args _) =
    (toUpper x : xs) ++ " " ++ unwords (map show args)

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

typeIdentifier :: Parser TypeExpr
typeIdentifier = do
  (x : xs) <- identifier
  return $ TypeName (toUpper x : xs)

typeExpr :: Parser TypeExpr
typeExpr = makeExprParser typeIdentifier operatorTable

operatorTable :: [[Operator Parser TypeExpr]]
operatorTable =
  [ [ Postfix (Star <$ symbol "*")
    , Postfix (Plus <$ symbol "+")
    , Postfix (Optional <$ symbol "?")
    ]
  , [InfixR (Product <$ symbol ",")]
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

groupByRes :: [Constructor] -> [[Constructor]]
groupByRes cs =
  let
    sorted :: [Constructor]
    sorted = sortOn output cs
  in groupBy ((==) `on` output) sorted

prettyPrint :: [ESLStmt] -> String
prettyPrint stmts =
  let
    (aliases, constructors) = splitStmt stmts
    printedAliases          = map show aliases
    printedData             = map prettyPrintData (groupByRes constructors)
  in concat
    [ "-- Aliases\n"
    , unlines printedAliases
    , "\n-- Constructors\n"
    , unlines printedData
    ]

prettyPrintData :: [Constructor] -> String
prettyPrintData []  = ""
prettyPrintData [c] = "data " ++ name c ++ " = " ++ show c
prettyPrintData (c : cs) =
  let start = "data " ++ show (output c) ++ "\n  = " ++ show c ++ "\n"
  in start ++ unlines (map (("  | " ++) . show) cs)
