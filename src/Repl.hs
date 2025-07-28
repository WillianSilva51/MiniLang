module Repl (runRepl) where

import Parser (parseExpr)
import Evaluator (eval)
import System.IO (hFlush, stdout)
import Text.Megaparsec.Error (errorBundlePretty)

runRepl :: IO ()
runRepl = do
  putStrLn "Welcome to the MiniLang REPL!"
  putStrLn "Type an expression, or ':quit' to exit."
  replLoop

replLoop :: IO ()
replLoop = do
  putStr "> "
  hFlush stdout

  input <- getLine
  if input == ":quit" || input == ":q"
    then putStrLn "Goodbye!"
  else do
    case parseExpr input of
      Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
      Right expr -> 
          case eval expr of
            Nothing -> putStrLn "Error: Invalid operation (e.g., division by zero, negative exponent, etc.)"
            Just result -> print result
    replLoop