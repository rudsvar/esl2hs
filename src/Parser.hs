-- | A class for parsing ESL and building ASTs.
module Parser
  ( parse
  , parseTest
  , errorBundlePretty
  , esl
  ) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad (void)
import Data.Void
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe (fromJust)

import Types
import Util

-- | The parser type
type Parser = Parsec Void String

-- | Consume spaces
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- | Create a new parser that consumes trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a string with trailing whitespace
symbol :: String -> Parser String
symbol = L.symbol sc

-- | The keywords
keywords :: [String]
keywords = ["type", "symbol"]

-- | Parse a keyword
keyword :: String -> Parser ()
keyword = void . symbol

-- | Parse something with surrounding parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse an identifier
identifier :: Parser String
identifier = label "identifier" $ lexeme $ do
  first <- letterChar
  rest  <- many alphaNumChar
  let name = first : rest
  if name `elem` keywords
    then fail $ show name ++ " is a keyword"
    else return name

-- | Parse a symbol statement
symbolDef :: Parser Symbol
symbolDef = do
  keyword "symbol"
  name <- identifier
  keyword ":"
  e1 <- typeExpr `sepBy` symbol ","
  keyword "->"
  e2 <- typeExpr
  return $ Symbol name e1 e2

-- | Parse a type alias statement
aliasDef :: Parser Alias
aliasDef = do
  keyword "type"
  i <- identifier
  keyword "="
  e <- typeExpr
  return $ Alias i e

-- | Parse a type term
typeTerm :: Parser TypeExpr
typeTerm = tName <|> tuple
 where
  tName = TypeName . upperFirst <$> identifier
  tuple = Product <$> parens (typeExpr `sepBy` symbol ",")

-- | Parse a type expression
typeExpr :: Parser TypeExpr
typeExpr = makeExprParser typeTerm operatorTable

-- | The operator table which defines the three
-- postfix operators * (zero or more), + (one or more),
-- and ? (optional).
operatorTable :: [[Operator Parser TypeExpr]]
operatorTable =
  [ [ Postfix (Star <$ symbol "*")
    , Postfix (Plus <$ symbol "+")
    , Postfix (Optional <$ symbol "?")
    ]
  ]

-- | Parse an ESL statement
eslStmt :: Parser ESLStmt
eslStmt = (S <$> symbolDef) <|> (A <$> aliasDef)

-- | Parse a sequence of ESL statements
esl :: Parser Module
esl = sc *> (makeModule <$> many eslStmt) <* eof

-- | Create a module from a list of ESL statements
makeModule :: [ESLStmt] -> Module
makeModule stmts =
  let
    (as, ctors) = splitStmt stmts
    grouped     = groupByOutput ctors
  in Module as (map makeDatatype grouped)

-- | Create a data type from symbol definitions
makeDatatype :: NonEmpty Symbol -> DataType
makeDatatype (c :| cs) = DataType (pretty $ output c) (c :| cs)

-- | Split statements into aliases and symbols
splitStmt :: [ESLStmt] -> ([Alias], [Symbol])
splitStmt [] = ([], [])
splitStmt (stmt : stmts) | (aliases, symbols) <- splitStmt stmts = case stmt of
  A a -> (a : aliases, symbols)
  S s -> (aliases, s : symbols)

-- | Group symbols by their output
-- `groupBy` can not return a list with empty lists in it,
-- so we use this to get a list of non-empty lists instead.
-- This is useful later, since datatypes must have at least one constructor
-- (other than `Data.Void`).
groupByOutput :: [Symbol] -> [NonEmpty Symbol]
groupByOutput cs =
  let
    sorted :: [Symbol]
    sorted = sortOn output cs
  in map (fromJust . nonEmpty) $ groupBy ((==) `on` output) sorted
