module Evaluator
    ( eval
    ) where

import Ast

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