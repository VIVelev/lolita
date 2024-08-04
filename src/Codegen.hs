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
            genFunctions h (definitions prog)
            -- genMain h (form prog)
            genTrailer h
            hClose h
    Left err -> print err

genHeader :: Handle -> P.SExp -> IO ()
genHeader h expr = do
  hPutStrLn h "/* Compiler to C, Version 0.1 */"
  hPrintf h "/* Source expression:\n  %s */\n" (show expr)
  hPutStrLn h "#include \"scheme.h\""
  hPutStrLn h ""

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
      hPrintf h "#define thing%d SCM_Wrap(&thing%d_object)\n" index index
      scanQuotations h rest i (q : done)
    genNew (P.Atom (P.Symbol sym)) =
      case find (\(Quotation _ v) -> v == P.Atom (P.StringLiteral sym)) done of
        Just (Quotation otherIndex _) -> do
          hPrintf h "SCM_DefineSymbol(thing%d_object, thing%d); /* %s */\n" index otherIndex sym
          hPrintf h "#define thing%d SCM_Wrap(&thing%d_object)\n" index index
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
            hPrintf h "#define thing%d SCM_Wrap(&thing%d_object)\n" index index
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

toC :: Handle -> Program IsFreeMutableOrQuote IndexFreeVars -> IO ()
toC _ (Const _) = error "Const can't be compiled to C."
-- TODO: Handle boxes
toC h (Reference v) =
  let name' = name v
   in case vInfo v of
        Left _ -> hPrintf h "thing%s" name'
        Right (IsFreeMutable isFree _)
          | isFree -> hPrintf h "SCM_Free(%s)" name'
          | otherwise -> hPutStr h name'
toC h (Assignment v p) = do
  hPrintf h "%s=" (name v)
  toC h p
toC h (Alternative c t f) = do
  boolToC c
  hPutStr h " ? "
  toC h t
  hPutStr h " : "
  toC h f
  where
    boolToC e = do
      toC h e
      hPutStr h " != SCM_false"
toC h (Sequence ps) = foldl1 (>>) (map (toC h) ps)
toC h (Function vars _ (IndexFreeVars index freeVars)) =
  let arity = length vars
      size = length freeVars
   in do
        hPrintf
          h
          "SCM_close(SCM_CfunctionAddress(function%d), %d, %d"
          index
          arity
          size
        compileVars freeVars
        hPutStr h ")"
  where
    compileVars :: [Variable IsFreeMutableOrQuote] -> IO ()
    compileVars (a : as) = do
      hPrintf h ", %s" (name a)
      compileVars as
    compileVars [] = pure ()
toC h (Application func args) =
  let n = length args
   in do
        hPrintf h "SCM_invoke(%d, " n
        toC h func
        compileArgs args
        hPutStrLn h ");"
  where
    compileArgs (a : as) = do
      hPutStr h ", "
      toC h a
      compileArgs as
    compileArgs [] = pure ()
toC _ (Magic _) = error "Magic keywords should have been eliminated."
toC _ (QuasiQuote {}) = error "Quasi quotes should hae been eliminated."

defineClosure :: Handle -> FunctionDefinition -> IO ()
defineClosure h (FunctionDefinition _ _ freeVars index) = do
  hPrintf h "SCM_DefineClosure(function%d, " index
  mapM_ (hPrintf h "SCM %s; " . name) freeVars
  hPutStrLn h ");\n"

declareFunction :: Handle -> FunctionDefinition -> IO ()
declareFunction h (FunctionDefinition vars body _ index) = do
  hPrintf h "SCM_DeclareFunction(function%d) {\n" index
  mapM_ (hPrintf h "  SCM_DeclareLocalVariable(%s);\n" . name) vars
  hPutStr h "  return "
  mapM_ (toC h) body
  hPutStrLn h "}\n"

genFunctions :: Handle -> [FunctionDefinition] -> IO ()
genFunctions h ds = do
  hPutStrLn h "/* Functions */"
  mapM_ (\d -> defineClosure h d >> declareFunction h d) (reverse ds)
