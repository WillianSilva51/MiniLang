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
--   * @Fact x@: fatorial de x
--   * @Fib x@: n-ésimo número de Fibonacci
--   * @Eq x y@: igualdade
--   * @Neq x y@: desigualdade
--   * @Lt x y@: menor que
--   * @Gt x y@: maior que
--   * @Le x y@: menor ou igual a
--   * @Ge x y@: maior ou igual a
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
  | Fact Expr
  | Fib Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Le Expr Expr
  | Ge Expr Expr

  -- Bitwise operations (Not yet implemented)
  | And Expr Expr
  | Or Expr Expr
  | Xor Expr Expr
  | Shl Expr Expr
  | Shr Expr Expr
  | Not Expr
  deriving (Read, Show, Ord, Eq)
