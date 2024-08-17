module Codegen where

import Control.Monad (unless)
import Data.Foldable (find)
import Data.String (IsString (fromString))
import Objectify
import Parse qualified as P (AKind (..), SExp (..))
import System.IO
import Text.Printf
import Walk

compileToC :: P.SExp -> String -> IO ()
compileToC e outname =
  case runObjectify (objectify e) defaultPrepEnv of
    Right (obj, globs) ->
      let prog = extract . free . markMutable . recordMutable . primitive $ obj
       in do
            h <- openFile (outname ++ ".c") WriteMode
            genHeader h e
            genGlobEnv h globs
            genQuotations h (quotations prog)
            genFunctions h (definitions prog)
            genMain h (form prog)
            genTrailer h
            hClose h
    Left err -> print err

genHeader :: Handle -> P.SExp -> IO ()
genHeader h expr = do
  hPutStrLn h "/* Compiler to C, Version 0.2 */"
  hPrintf h "/* Source expression:\n  %s */\n\n" (show expr)
  hPutStrLn h "#include \"scheme.h\""
  hPutStrLn h ""

genTrailer :: Handle -> IO ()
genTrailer h = do
  hPutStrLn h "/* End of generated code. */"

genGlobEnv :: Handle -> [Variable ()] -> IO ()
genGlobEnv h globals = unless (null globals) $ do
  hPutStrLn h "/* Global environment: */"
  foldl (>>) (pure ()) (map (genGlobVar h) (filter notPrimitive globals))
  hPutStrLn h ""
  where
    notPrimitive (Variable {name = name}) =
      case (fromString :: String -> Maybe Primitive) name of
        Just _ -> False
        Nothing -> True

genGlobVar :: Handle -> Variable () -> IO ()
genGlobVar h var =
  hPrintf
    h
    "SCM_DefineGlobalVariable(%s, \"%s\");\n"
    (schemeIdToC (name var))
    (name var)

-- TODO: Complete this implementation lol.
schemeIdToC :: String -> String
schemeIdToC = map (\c -> if c == '-' then '_' else c)

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

toC :: Handle -> Program IsFreeMutablePrimitiveOrQuote IndexFreeVars -> IO ()
toC _ (Const _) = error "Const can't be compiled to C."
-- TODO: Handle boxes
toC h (Reference v) =
  let name' = name v
   in case vInfo v of
        Left _ -> hPrintf h "thing%s" name'
        Right (IsFreeMutablePrimitive isFree _ _)
          | isFree -> hPrintf h "SCM_Free(%s)" (schemeIdToC name')
          | otherwise -> hPutStr h (schemeIdToC name')
toC h (Assignment v p) = do
  hPrintf h "%s=" (schemeIdToC . name $ v)
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
toC h (Sequence body) = do
  hPutStr h "("
  mapM_ (\e -> toC h e >> hPutStr h ", ") (take (length body - 1) body)
  toC h (last body)
  hPutStr h ")"
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
        compileVars (filter notPrimitive freeVars)
        hPutStr h ")"
  where
    compileVars :: [Variable IsFreeMutablePrimitiveOrQuote] -> IO ()
    compileVars (a : as) = do
      hPrintf h ", %s" (schemeIdToC . name $ a)
      compileVars as
    compileVars [] = pure ()
    notPrimitive (Variable {vInfo = Right (IsFreeMutablePrimitive _ _ Nothing)}) = True
    notPrimitive _ = False
-- \| Primitive function application
toC h (Application (Reference (Variable _ _ (Right (IsFreeMutablePrimitive _ _ (Just prim))))) args) =
  case prim of
    Plus -> do
      hPutStr h "SCM_Add("
      compileArgs args
      hPutStr h ")"
    Minus -> do
      hPutStr h "SCM_Sub("
      compileArgs args
      hPutStr h ")"
    Times -> do
      hPutStr h "SCM_Times("
      compileArgs args
      hPutStr h ")"
    Eqn -> do
      hPutStr h "SCM_Eqn("
      compileArgs args
      hPutStr h ")"
  where
    compileArgs [a] = do
      toC h a
    compileArgs (a : as) = do
      toC h a
      hPutStr h ", "
      compileArgs as
    compileArgs [] = pure ()
-- \| Non primitive function application
toC h (Application func args) =
  let n = length args
   in do
        hPutStr h "SCM_invoke("
        toC h func
        hPrintf h ", %d" n
        compileArgs args
        hPutStr h ")"
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
  mapM_
    (hPrintf h "SCM %s; " . schemeIdToC . name)
    (filter notPrimitive freeVars)
  hPutStrLn h ");\n"
  where
    notPrimitive (Variable {vInfo = Right (IsFreeMutablePrimitive _ _ Nothing)}) = True
    notPrimitive _ = False

declareFunction :: Handle -> FunctionDefinition -> IO ()
declareFunction h (FunctionDefinition vars body _ index) = do
  hPrintf h "SCM_DeclareFunction(function%d) {\n" index
  mapM_ (hPrintf h "  SCM_DeclareLocalVariable(%s);\n" . schemeIdToC . name) vars
  mapM_ (\e -> hPutStr h "  " >> toC h e >> hPutStrLn h ";") (take (length body - 1) body)
  hPutStr h "  return "
  toC h . last $ body
  hPutStr h ";"
  hPutStrLn h "\n}\n"

genFunctions :: Handle -> [FunctionDefinition] -> IO ()
genFunctions h ds = do
  hPutStrLn h "/* Functions */"
  mapM_ (\d -> defineClosure h d >> declareFunction h d) (reverse ds)

genMain :: Handle -> Program IsFreeMutablePrimitiveOrQuote IndexFreeVars -> IO ()
genMain h form = do
  hPutStrLn h "/* Main */"
  hPutStrLn h "int main(void) {"
  hPutStr h "  SCM_print("
  toC h form
  hPutStrLn h ");"
  hPutStrLn h "  exit(0);\n}\n"
