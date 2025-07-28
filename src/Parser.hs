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
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme p = p <* sc

symbol :: String -> Parser String
symbol s = lexeme (string s)

pInteger :: Parser Integer
pInteger = lexeme L.decimal

pLiteral :: Parser Expr
pLiteral = Lit <$> pInteger

pTerm :: Parser Expr
pTerm = pParens <|> pFunctionCall <|> pLiteral

pParens :: Parser Expr
pParens = between (symbol "(") (symbol ")") pExpr

pFunctionCall :: Parser Expr
pFunctionCall = pGcdCall <|> pLcmCall
    where
        pGcdCall = ((GCD <$ symbol "gcd") <*> pTerm) <*> pTerm
        pLcmCall = ((LCM <$ symbol "lcm") <*> pTerm) <*> pTerm

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorsTable
    where
        operatorsTable :: [[Operator Parser Expr]]
        operatorsTable = [
            -- Unary operators
            [
                Postfix (Fact <$ symbol "!"),
                Prefix (Fib <$ symbol "fib")
            ],
            -- Exponentiation and right-to-left exponentiation
            [
                InfixR (Exp <$ symbol "**"),
                InfixR (Rt <$ symbol "#")
            ],
            -- Multiplication, division, and modulus
            [
                InfixL (Mul <$ symbol "*"),
                InfixL (Div <$ symbol "/"),
                InfixL (Mod <$ symbol "%")
            ],
            -- Addition and subtraction
            [
                InfixL (Add <$ symbol "+"),
                InfixL (Sub <$ symbol "-")
            ],
            -- Relational operators
            [
                InfixL (Eq <$ symbol "=="),
                InfixL (Neq <$ symbol "!="),
                InfixL (Lt <$ symbol "<"),
                InfixL (Gt <$ symbol ">"),
                InfixL (Le <$ symbol "<="),
                InfixL (Ge <$ symbol ">=")
            ],
            -- bitwise operators
            [
                InfixL (And <$ symbol "&"),
                InfixL (Or <$ symbol "|"),
                InfixL (Xor <$ symbol "^"),
                InfixL (Shl <$ symbol "<<"),
                InfixL (Shr <$ symbol ">>"),
                Prefix (Not <$ symbol "~")
            ]
            ]

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (sc *> pExpr <* eof) "minilang-input"
