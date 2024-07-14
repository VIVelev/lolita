module Codegen where

import Control.Monad (unless)
import Data.Foldable (find)
import Objectify
import Parse qualified as P (AKind (..), SExp (..))
import System.IO
import Text.Printf
import Walk

compileToC :: P.SExp -> String -> IO ()
compileToC e outname =
  case runObjectify (objectify e) defaultPrepEnv of
    Right (obj, globs) ->
      let prog = extract . free . markMutable . recordMutable $ obj
       in do
            h <- openFile (outname ++ ".c") WriteMode
            genHeader h e
            genGlobEnv h globs
            genQuotations h (quotations prog)
            genTrailer h
            hClose h
    Left err -> print err

genHeader :: Handle -> P.SExp -> IO ()
genHeader h expr = do
  hPutStrLn h "/* Compiler to C, Version 0.1 */"
  hPrintf h "/* Source expression:\n  %s */\n\n" (show expr)

genTrailer :: Handle -> IO ()
genTrailer h = do
  hPutStrLn h "/* End of generated code. */"

genGlobEnv :: Handle -> [Variable ()] -> IO ()
genGlobEnv h globals = unless (null globals) $ do
  hPutStrLn h "/* Global environment: */"
  foldl1 (>>) (map (genGlobVar h) globals)
  hPutStrLn h ""

genGlobVar :: Handle -> Variable () -> IO ()
genGlobVar h var =
  hPrintf
    h
    "SCM_DefineGlobalVariable(%s, \"%s\");\n"
    (schemeIdToC (name var))
    (name var)

-- TODO: Complete this implementation lol.
schemeIdToC :: String -> String
schemeIdToC = id

genQuotations :: Handle -> [Quotation] -> IO ()
genQuotations h qs = unless (null qs) $ do
  hPutStrLn h "/* Quotations */"
  scanQuotations h qs (length qs) []
  hPutStrLn h ""

scanQuotations :: Handle -> [Quotation] -> Int -> [Quotation] -> IO ()
scanQuotations h qs@(q@(Quotation index value) : rest) i done =
  case find (\(Quotation _ v) -> v == value) done of
    Just (Quotation otherIndex _) -> do
      genAlias otherIndex
      scanQuotations h rest i (q : done)
    Nothing -> genNew value
  where
    genNew (P.Atom (P.IntLiteral val)) = do
      hPrintf h "#define thing%d SCM_Int2fixnum(%d)\n" index val
      scanQuotations h rest i (q : done)
    genNew (P.Atom (P.BoolLiteral b)) = do
      hPrintf h "#define thing%d %s\n" index (if b then "SCM_true" else "SCM_false")
      scanQuotations h rest i (q : done)
    genNew (P.Atom (P.StringLiteral str)) = do
      hPrintf h "SCM_DefineString(thing%d_object, \"%s\");\n" index str
      hPrintf h "#define thing%d SCM_Wrap(thing%d_object)\n" index index
      scanQuotations h rest i (q : done)
    genNew (P.Atom (P.Symbol sym)) =
      case find (\(Quotation _ v) -> v == P.Atom (P.StringLiteral sym)) done of
        Just (Quotation otherIndex _) -> do
          hPrintf h "SCM_DefineSymbol(thing%d_object, thing%d); /* %s */\n" index otherIndex sym
          hPrintf h "#define thing%d SCM_Wrap(thing%d_object)\n" index index
          scanQuotations h rest i (q : done)
        Nothing ->
          let newq = Quotation i (P.Atom (P.StringLiteral sym))
           in scanQuotations h (newq : qs) (i + 1) done
    genNew P.Nil = do
      hPrintf h "#define thing%d SCM_nil\n" index
      scanQuotations h rest i (q : done)
    genNew (P.Pair a d) =
      case find (\(Quotation _ v) -> v == d) done of
        Just (Quotation dIndex _) -> case find (\(Quotation _ v) -> v == a) done of
          Just (Quotation aIndex _) -> do
            hPrintf
              h
              "SCM_DefinePair(thing%d_object, thing%d, thing%d); /* %s */\n"
              index
              aIndex
              dIndex
              (show value)
            hPrintf h "#define thing%d SCM_Wrap(thing%d_object)\n" index index
            scanQuotations h rest i (q : done)
          Nothing ->
            let newq = Quotation i a
             in scanQuotations h (newq : qs) (i + 1) done
        Nothing ->
          let newq = Quotation i d
           in scanQuotations h (newq : qs) (i + 1) done

    genAlias to =
      hPrintf h "#define thing%d thing%d /* %s */\n" index to (show value)
scanQuotations _ [] _ _ = pure ()
