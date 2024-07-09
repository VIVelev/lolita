module Main where

import ParseTest qualified
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import WalkTest qualified

main :: IO ()
main = do
  pCounts <- ParseTest.run
  wCounts <- WalkTest.run
  let counts = [pCounts, wCounts]
  if sum (map errors counts) == 0 && sum (map failures counts) == 0
    then exitSuccess
    else exitFailure
