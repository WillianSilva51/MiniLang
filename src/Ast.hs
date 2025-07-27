-- | Árvores de sintaxe abstrata (AST) da linguagem MiniLang.
module Ast
    ( Expr(..)) where

-- | 'Expr' representa expressões aritméticas básicas.
--
--   * @Lit n@: literal inteiro  
--   * @Add x y@: adição  
--   * @Sub x y@: subtração  
--   * @Mul x y@: multiplicação  
--   * @Div x y@: divisão inteira  
--   * @Mod x y@: resto da divisão
--   * @Exp x y@: exponenciação
--   * @Rt x y@: A raiz de x por y
--   * @GCD x y@: máximo divisor comum de x e y
--   * @LCM x y@: mínimo múltiplo comum de x e y
data Expr =
    Lit Integer
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Exp Expr Expr
  | Rt Expr Expr
  | GCD Expr Expr
  | LCM Expr Expr
  deriving (Read, Show, Ord, Eq)
