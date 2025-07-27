module Main (main) where

import qualified Repl (runRepl)

main :: IO ()
main = do
  Repl.runRepl
