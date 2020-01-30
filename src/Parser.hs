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

import Types
import Util

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
typeName = TypeName . upperFirst <$> identifier

typeTerm :: Parser TypeExpr
typeTerm = typeName <|> tuple

tuple :: Parser TypeExpr
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

eslStmt :: Parser ESLStmt
eslStmt = (C <$> constructor) <|> (A <$> alias)

esl :: Parser [ESLStmt]
esl = many eslStmt <* eof
