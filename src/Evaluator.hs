{-|
Módulo      : Evaluator
Descrição   : Fornece um avaliador para expressões aritméticas e de comparação definidas no módulo Ast.
-}
module Evaluator
    ( eval
    ) where

import Ast
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor, complement)

{-| 
A função 'eval' avalia recursivamente expressões do tipo 'Expr', retornando um resultado 'Maybe Integer'.
Operações suportadas incluem:

* Literais ('Lit')
* Aritméticas: adição ('Add'), subtração ('Sub'), multiplicação ('Mul'), divisão ('Div'), módulo ('Mod'), exponenciação ('Exp'), raiz ('Rt')
* Teoria dos números: máximo divisor comum ('GCD'), mínimo múltiplo comum ('LCM'), fatorial ('Fact'), Fibonacci ('Fib')
* Comparações: igual ('Eq'), diferente ('Neq'), menor que ('Lt'), maior que ('Gt'), menor ou igual ('Le'), maior ou igual ('Ge')

Divisão e módulo por zero, expoentes negativos e raízes inválidas retornam 'Nothing'.
Fatorial e Fibonacci são definidos apenas para inteiros não negativos.
-}
eval :: Expr -> Maybe Integer
eval (Lit x) = Just x

eval (Add x y) = do
    a <- eval x
    b <- eval y
    return (a + b)

eval (Sub x y) = do
    a <- eval x
    b <- eval y
    return (a - b)

eval (Mul x y) = do
    a <- eval x
    b <- eval y
    return (a * b)

eval (Div x y) = do
    a <- eval x
    b <- eval y
    if b == 0 then 
        Nothing
    else
        Just (a `div` b)

eval (Mod x y) = do
    a <- eval x
    b <- eval y
    if b == 0 then
        Nothing
    else
        Just (a `mod` b)

eval (Exp x y) = do
    a <- eval x
    b <- eval y
    if b < 0 then 
        Nothing
    else 
        Just (a ^ b)

eval (Rt x y) = do
    a <- eval x
    b <- eval y
    if b == 0 || (a < 0 && even b) then
        Nothing
    else 
        Just (round (fromIntegral a ** ((1.0 :: Double) / fromIntegral b)))

eval (GCD x y) = do
    a <- eval x
    b <- eval y
    return (gcd a b)

eval (LCM x y) = do
    a <- eval x
    b <- eval y
    return (lcm a b)

eval (Fact x) = do
    a <- eval x
    if a < 0 then
        Nothing
    else
        Just (factorial a)
        where
            factorial :: Integer -> Integer 
            factorial 0 = 1
            factorial n = n * factorial (n - 1)

eval (Fib x) = do
    a <- eval x
    if a < 0 then
        Nothing
    else
        Just (fib a)
        where
            fib :: Integer -> Integer
            fib 0 = 0
            fib 1 = 1
            fib n = fib (n - 1) + fib (n - 2)

eval (Eq x y) = do
    a <- eval x
    b <- eval y
    return (if a == b then 1 else 0)

eval (Neq x y) = do
    a <- eval x
    b <- eval y
    return (if a /= b then 1 else 0)

eval (Lt x y) = do
    a <- eval x
    b <- eval y
    return (if a < b then 1 else 0)

eval (Gt x y) = do
    a <- eval x
    b <- eval y
    return (if a > b then 1 else 0)

eval (Le x y) = do
    a <- eval x
    b <- eval y
    return (if a <= b then 1 else 0)

eval (Ge x y) = do
    a <- eval x
    b <- eval y
    return (if a >= b then 1 else 0)

-- Bitwise operations
eval (And x y) = do
    a <- eval x
    b <- eval y
    return (a .&. b)

eval (Or x y) = do
    a <- eval x
    b <- eval y
    return (a .|. b)

eval (Xor x y) = do
    a <- eval x
    b <- eval y
    return (a `xor` b)

eval (Shl x y) = do
    a <- eval x
    b <- eval y
    return (a `shiftL` fromIntegral b)

eval (Shr x y) = do
    a <- eval x
    b <- eval y
    return (a `shiftR` fromIntegral b)

eval (Not x) = do
    a <- eval x
    return (complement a)