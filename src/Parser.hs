module Parser
    ( parseExpr
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Ast

-- | Tipo de parser usando Megaparsec
type Parser = Parsec Void String

sc :: Parser ()

lexeme :: Parser a -> Parser a

symbol :: String -> Parser String

pInteger :: Parser Integer

pLiteral :: Parser Expr

pParens :: Parser Expr

pTerm :: Parser Expr

pExpr :: Parser Expr

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
