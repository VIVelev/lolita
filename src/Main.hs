module Main where

import Data.String (IsString (fromString))
import Objectify qualified as O (defaultPrepEnv, objectify, runObjectify)
import Parse qualified as P (runParser, sexp)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      case P.runParser P.sexp $ fromString contents of
        Right expr -> case O.runObjectify (O.objectify expr) O.defaultPrepEnv of
          Right obj -> print obj
          Left e -> print e
        Left e -> print e
    _ -> putStrLn "Usage: program <filename>"
