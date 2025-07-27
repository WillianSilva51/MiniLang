module Main (main) where

import Test.Hspec
import Ast
import Evaluator
import Parser

main :: IO ()
main = hspec $ do
  describe "Evaluator" $ do
    it "avalia literais simples" $
      eval (Lit 5) `shouldBe` Just 5

    it "avalia uma soma simples" $
      eval (Add (Lit 2) (Lit 3)) `shouldBe` Just 5

    it "retorna Nothing para divisão por zero" $
      eval (Div (Lit 10) (Lit 0)) `shouldBe` Nothing

  describe "Parser" $ do
    it "faz parsing de um número" $
      parseExpr "  123  " `shouldBe` Right (Lit 123)
      
    it "faz parsing de uma expressão com precedência" $
      parseExpr "2 + 3 * 4" `shouldBe` Right (Add (Lit 2) (Mul (Lit 3) (Lit 4)))