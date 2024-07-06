{-# LANGUAGE LambdaCase #-}

-- | A Tower of Evaluators LISP interpreter.
--
-- Reference:
--   - LISP in Small Pieces ...
module Eval where

import MonadT
  ( ErrorT (..),
    Identity (..),
    MonadError (..),
    MonadReader (..),
    MonadState (..),
    ReaderT (..),
    StateT (..),
  )
import Parse qualified as P

type Error = String

data PrepEnv = PrepEnv
  { variables :: [(String, Variable)],
    keywords :: [(String, Keyword)]
  }

type Objectify = ErrorT String (StateT PrepEnv Identity)

runObjectify :: Objectify a -> PrepEnv -> Either Error a
runObjectify obj env = fst <$> runIdentity $ runStateT (runErrorT obj) env

type RunTimeEnv = [(Variable, P.SExp)]

type Evaluate = ErrorT String (ReaderT RunTimeEnv Identity)

runEvaluate :: Evaluate a -> RunTimeEnv -> Either Error a
runEvaluate evl env = runIdentity $ runReaderT (runErrorT evl) env

data Keyword = Keyword
  { symbol :: String,
    handler :: P.SExp -> Objectify Program
  }

instance Show Keyword where
  show k = "<keyword: " ++ show (symbol k) ++ ">"

instance Eq Keyword where
  a == b = symbol a == symbol b

data Variable
  = LocalVariable
      { name :: String,
        isMutable :: Bool,
        isDotted :: Bool
      }
  | GlobalVariable {name :: String}
  deriving (Show, Eq)

data Program
  = IntLiteral Integer
  | BoolLiteral Bool
  | Nil
  | LocalReference Variable
  | -- | TODO: no need for to types of references
    GlobalReference Variable
  | Altarnative
      { condition :: Program,
        consequent :: Program,
        alternate :: Program
      }
  | Sequence [Program]
  | Function
      { vars :: [Variable],
        body :: Program
      }
  | -- | This is a special case, as you may guess from the name.
    Magic Keyword
  | -- | Quoting is vital for a language to support macros.
    Quote P.SExp
  | QuasiQuote
      { root :: P.SExp,
        unquotes :: [(P.SExp, Program)]
      }
  deriving (Show, Eq)

evaluate :: Program -> Evaluate P.SExp
evaluate (IntLiteral i) = return $ P.Atom (P.IntLiteral i)
evaluate (BoolLiteral b) = return $ P.Atom (P.BoolLiteral b)
evaluate Nil = return P.Nil
evaluate (LocalReference var) = do
  env <- ask
  case lookup var env of
    Just e -> return e
    Nothing -> throwError "unbound local variable"
evaluate (GlobalReference var) = do
  env <- ask
  case lookup var env of
    Just e -> return e
    Nothing -> throwError "unbound global variable"
evaluate (Altarnative ec et ef) = do
  b <- evaluate ec
  case b of
    (P.Atom (P.BoolLiteral True)) -> evaluate et
    (P.Atom (P.BoolLiteral False)) -> evaluate ef
    _ -> throwError "Value is not boolyfiable"
evaluate (Sequence p) = foldl1 (>>) (map evaluate p)
evaluate (Quote e) = return e
evaluate q@(QuasiQuote {root, unquotes}) =
  case root of
    P.Pair (P.Atom (P.Symbol "unquote")) (P.Pair x P.Nil) ->
      case lookup x unquotes of
        Just p -> evaluate p
        Nothing -> throwError $ "Could not unquote: " ++ show x
    P.Pair car cdr -> (P.Pair <$> evaluate q {root = car}) <*> evaluate q {root = cdr}
    _ -> pure root
evaluate _ = throwError "Not yet implemented"

objectify :: P.SExp -> Objectify Program
objectify (P.Atom (P.IntLiteral i)) = return $ IntLiteral i
objectify (P.Atom (P.BoolLiteral b)) = return $ BoolLiteral b
objectify (P.Atom (P.Symbol name)) = do
  env <- get
  case lookup name (variables env) of
    Just v@(LocalVariable {}) -> return $ LocalReference v
    Just v@(GlobalVariable _) -> return $ GlobalReference v
    Nothing -> case lookup name (keywords env) of
      Just k -> return $ Magic k
      Nothing ->
        let var = GlobalVariable name
            ref = GlobalReference var
         in do
              modify (\r@PrepEnv {variables} -> r {variables = (name, var) : variables})
              return ref
objectify P.Nil = return Nil
objectify e@(P.Pair car _) = do
  m <- objectify car
  case m of
    Magic k -> handler k e
    -- otherwise, its an application
    _ -> return Nil

-- | (if <condition> <consequent> <alternate>)
if_ :: Keyword
if_ =
  Keyword
    { symbol = "if",
      handler = \case
        P.Pair _ (P.Pair ce (P.Pair te (P.Pair fe P.Nil))) ->
          Altarnative <$> objectify ce <*> objectify te <*> objectify fe
        _ -> throwError "Invalid if syntax"
    }

-- | Objectifies a sequence of expressions
-- as if they are executed sequentially.
--
-- It returns a Sequence.
objectifySequence :: P.SExp -> Objectify Program
objectifySequence e =
  case P.listify e of
    Right es ->
      Sequence . reverse
        <$> foldl
          (\b a -> (:) <$> ((head <$> b) >> objectify a) <*> b)
          ((:) <$> objectify (head es) <*> pure [])
          (tail es)
    Left err -> throwError err

-- | Given s-expressions `(v1 v2 v3 ...)` and `(e1 e2 e3 ...)`
-- it returns the program `(begin e1 e2 e3)`, so that the program
-- referes to the variables v1, v2, etc.
--
-- It returns a Function.
objectifyWithScope :: P.SExp -> P.SExp -> Objectify Program
objectifyWithScope varsExp bodyExp =
  case P.listify varsExp of
    Left err -> throwError err
    Right list -> case toVariables list of
      Left err -> throwError err
      Right vars -> do
        body <- locally (extendEnv vars) (objectifySequence bodyExp)
        return $ Function vars body
  where
    toVariables ((P.Atom (P.Symbol name)) : xs) =
      (:) (LocalVariable name False False) <$> toVariables xs
    toVariables [] = Right []
    toVariables _ = Left "Invalid argument list, only symbols are allowed as lambda variables"
    extendEnv vars r@PrepEnv {variables} = r {variables = map (\x -> (name x, x)) vars <> variables}

-- | (lambda (<names> ...) body)
lambda :: Keyword
lambda =
  Keyword
    { symbol = "lambda",
      handler = \case
        P.Pair _ (P.Pair vars body) ->
          objectifyWithScope vars body
        _ -> throwError "Invalid lambda syntax"
    }

-- | (begin ...)
begin :: Keyword
begin =
  Keyword
    { symbol = "begin",
      handler = objectifySequence . P.cdr
    }

-- | (quote ...)
quote :: Keyword
quote =
  Keyword
    { symbol = "quote",
      handler = pure . Quote . P.cadr
    }

-- | (quasiquote ...)
quasiquote :: Keyword
quasiquote =
  Keyword
    { symbol = "quasiquote",
      handler = \case
        (P.Pair _ (P.Pair root P.Nil)) -> QuasiQuote root <$> collect root
        _ -> throwError "Invalid quasiquote syntax"
    }
  where
    collect (P.Pair (P.Atom (P.Symbol "unquote")) (P.Pair x P.Nil)) = (\p -> [(x, p)]) <$> objectify x
    collect (P.Pair car cdr) = (<>) <$> collect car <*> collect cdr
    collect _ = pure []

-- | (defmacro (name <variables>)
--     <body>)
defmacro :: Keyword
defmacro =
  Keyword
    { symbol = "defmacro",
      handler = \case
        P.Pair _ (P.Pair call body) ->
          case call of
            P.Pair (P.Atom (P.Symbol name)) vars -> do
              case runObjectify (objectifyWithScope vars body) defaultPrepEnv of
                Left err -> throwError $ "Error in macro definition: " ++ err
                Right expanderProgram ->
                  let newKeyword = Keyword {symbol = name, handler = invoke expanderProgram . P.cdr}
                   in withState
                        (\r@PrepEnv {keywords} -> r {keywords = (name, newKeyword) : keywords})
                        (pure $ BoolLiteral True)
            _ -> throwError "Invalid macro name: expected a symbol"
        _ -> throwError "Invalid defmacro syntax: expected (defmacro (name <variables>) <body>)"
    }

invoke :: Program -> P.SExp -> Objectify Program
invoke (Function vars body) args =
  case P.listify args of
    Right argList
      | length argList == length vars -> do
          let env = zip vars argList
          case runEvaluate (local (<> env) (evaluate body)) defaultRunTimeEnv of
            Left err -> throwError $ "Error in macro expansion: " ++ err
            Right res -> objectify res
      | otherwise ->
          throwError $
            "Macro invocation error: expected "
              ++ show (length vars)
              ++ " arguments, but got "
              ++ show (length (P.listify args))
    Left e -> throwError e
invoke _ _ = throwError "Internal error: expected a Function in macro invocation"

defaultPrepEnv :: PrepEnv
defaultPrepEnv =
  PrepEnv
    { variables = [],
      keywords =
        map
          (\x -> (symbol x, x))
          [ if_,
            lambda,
            begin,
            quote,
            quasiquote,
            defmacro
          ]
    }

defaultRunTimeEnv :: RunTimeEnv
defaultRunTimeEnv = []

eval :: P.SExp -> IO ()
eval e = case runObjectify (objectify e) defaultPrepEnv of
  Left err -> print $ "comptime errro: " ++ err
  Right obj -> case runEvaluate (evaluate obj) defaultRunTimeEnv of
    Left err -> print $ "runtime error: " ++ show err
    Right res -> print res
