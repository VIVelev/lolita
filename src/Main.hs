module Main where

import Codegen (compileToC)
import Data.String (IsString (fromString))
import Parse qualified as P (runParser, sexp)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      case P.runParser P.sexp $ fromString contents of
        Right expr -> compileToC expr "out"
        Left e -> print e
    _ -> putStrLn "Usage: program <filename>"
