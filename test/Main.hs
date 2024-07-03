module Main where

import ParseTest qualified
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
  counts <- ParseTest.run
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
