module Main where

import Data.String (IsString (fromString))
import Eval qualified as E (defaultPrepEnv, objectify, runObjectify)
import Parse qualified as P (State, runParser, sexp)
import System.Environment

prepContents :: String -> P.State
prepContents contents =
  fromString $
    fromString $
      "(begin " ++ contents ++ ")"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      case P.runParser P.sexp $ prepContents contents of
        Right expr -> case E.runObjectify (E.objectify expr) E.defaultPrepEnv of
          Right obj -> print obj
          Left e -> print e
        Left e -> print e
    _ -> putStrLn "Usage: program <filename>"
