{-# LANGUAGE LambdaCase #-}

-- | Turn S-expression into object tree that can be walked.
module Objectify where

import Data.String (IsString (fromString))
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

newtype LocalPrepEnv = LocalPrepEnv [(String, Variable ())]
  deriving (Semigroup, Monoid)

data GlobalPrepEnv = GlobalPrepEnv
  { variables :: [(String, Variable ())],
    keywords :: [(String, Keyword)]
  }

type ObjectifyM = ErrorT String (ReaderT LocalPrepEnv (StateT GlobalPrepEnv Identity))

runObjectify :: ObjectifyM a -> (LocalPrepEnv, GlobalPrepEnv) -> Either Error (a, [Variable ()])
runObjectify obj (l, g) = case runIdentity $ runStateT (runReaderT (runErrorT obj) l) g of
  (Right a, env) -> Right (a, map snd (variables env))
  (Left e, _) -> Left e

data RunTimeEnv = RunTimeEnv
  { rLocals :: [(Variable (), P.SExp)],
    rGlobals :: [(Variable (), P.SExp)]
  }

type Evaluate = ErrorT String (ReaderT RunTimeEnv Identity)

runEvaluate :: Evaluate a -> RunTimeEnv -> Either Error a
runEvaluate evl env = runIdentity $ runReaderT (runErrorT evl) env

data Keyword = Keyword
  { symbol :: String,
    handler :: P.SExp -> ObjectifyM (Program () ())
  }

instance Show Keyword where
  show k = "<keyword: " ++ show (symbol k) ++ ">"

instance Eq Keyword where
  a == b = symbol a == symbol b

data Variable v = Variable
  { name :: String,
    isGlobal :: Bool,
    -- to be filled by code walkers
    vInfo :: v
  }
  deriving (Eq)

instance Show (Variable ()) where
  show (Variable name False _) = name
  show (Variable name True _) = "g:" ++ name

data Program v f
  = Const P.SExp
  | Reference (Variable v)
  | Assignment (Variable v) (Program v f)
  | Alternative
      { condition :: Program v f,
        consequent :: Program v f,
        alternate :: Program v f
      }
  | Sequence [Program v f]
  | Function
      { vars :: [Variable v],
        body :: [Program v f],
        -- | to be filled by code walkers
        fInfo :: f
      }
  | Application
      { func :: Program v f,
        args :: [Program v f]
      }
  | -- | This is a special case, as you may guess from the name.
    Magic Keyword
  | -- | Quoting is vital for a language to support macros.
    QuasiQuote
      { root :: P.SExp,
        unquotes :: [(P.SExp, Program v f)]
      }
  deriving (Eq)

deriving instance Show (Program () ())

evaluate :: Program () () -> Evaluate P.SExp
evaluate (Const e) = return e
evaluate (Reference var) = do
  env <- ask
  case lookup var (rLocals env) of
    Just e -> return e
    Nothing -> case lookup var (rGlobals env) of
      Just e -> return e
      Nothing -> throwError "unbound local variable"
evaluate (Alternative ec et ef) = do
  b <- evaluate ec
  case b of
    (P.Atom (P.BoolLiteral True)) -> evaluate et
    (P.Atom (P.BoolLiteral False)) -> evaluate ef
    _ -> throwError "Value is not boolyfiable"
evaluate (Sequence p) = foldl1 (>>) (map evaluate p)
evaluate q@(QuasiQuote {root, unquotes}) =
  case root of
    P.Pair (P.Atom (P.Symbol "unquote")) (P.Pair x P.Nil) ->
      case lookup x unquotes of
        Just p -> evaluate p
        Nothing -> throwError $ "Could not unquote: " ++ show x
    P.Pair car cdr -> (P.Pair <$> evaluate q {root = car}) <*> evaluate q {root = cdr}
    _ -> pure root
evaluate _ = throwError "Not yet implemented"

objectify :: P.SExp -> ObjectifyM (Program () ())
objectify q@(P.Atom (P.IntLiteral _)) = return $ Const q
objectify q@(P.Atom (P.BoolLiteral _)) = return $ Const q
objectify q@(P.Atom (P.StringLiteral _)) = return $ Const q
objectify q@P.Nil = return $ Const q
objectify (P.Atom (P.Symbol name)) = do
  LocalPrepEnv locals <- ask
  case lookup name locals of
    Just v@(Variable {}) -> return $ Reference v
    Nothing -> do
      GlobalPrepEnv {variables, keywords} <- get
      case lookup name variables of
        Just v@(Variable {}) -> return $ Reference v
        Nothing -> case lookup name keywords of
          Just k -> return $ Magic k
          Nothing ->
            let var = Variable name True ()
                ref = Reference var
             in do
                  modify (\r@GlobalPrepEnv {variables = vs} -> r {variables = (name, var) : vs})
                  return ref
objectify e@(P.Pair car cdr) = do
  m <- objectify car
  case m of
    Magic k -> handler k e
    -- otherwise it's an application
    _ -> do
      case P.listify cdr of
        Right args -> Application m <$> mapM objectify args
        Left err -> throwError $ "Invalid function application: " ++ err

-- | (if <condition> <consequent> <alternate>)
if_ :: Keyword
if_ =
  Keyword
    { symbol = "if",
      handler = \case
        P.Pair _ (P.Pair ce (P.Pair te (P.Pair fe P.Nil))) ->
          Alternative <$> objectify ce <*> objectify te <*> objectify fe
        _ -> throwError "Invalid if syntax"
    }

-- | Given s-expressions `(v1 v2 v3 ...)` and `(e1 e2 e3 ...)`
-- it returns the program `(begin e1 e2 e3)`, so that the program
-- referes to the variables v1, v2, etc.
--
-- It returns a Function.
objectifyWithScope :: P.SExp -> P.SExp -> ObjectifyM (Program () ())
objectifyWithScope varsExp bodyExp =
  case parseVars varsExp of
    Right vars ->
      case P.listify bodyExp of
        Right es -> do
          body <- local (LocalPrepEnv (zip (map name vars) vars) <>) (mapM objectify es)
          return $ Function vars body ()
        Left err -> throwError $ "in body: " ++ show err
    Left err -> throwError $ "in variable list: " ++ show err
  where
    parseVars = \case
      P.Nil -> Right []
      P.Pair (P.Atom (P.Symbol name)) rest ->
        Right (Variable name False () :) <*> parseVars rest
      _ -> Left "Invalid argument list, only symbols are allowed as lambda variables"

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
      handler = \case
        P.Pair _ rest -> case P.listify rest of
          Right es -> Sequence <$> mapM objectify es
          Left err -> throwError $ "Invalid begin syntax: " ++ show err
        _ -> throwError "Invalid begin syntax"
    }

-- | (set! <name> form)
set :: Keyword
set =
  Keyword
    { symbol = "set!",
      handler = \case
        (P.Pair _ (P.Pair n (P.Pair f P.Nil))) ->
          do
            ref <- objectify n
            form <- objectify f
            case ref of
              Reference var -> return $ Assignment var form
              _ -> throwError $ "Cannot set: " ++ show n
        _ -> throwError "Invalid set! syntax"
    }

-- | (quote ...)
quote :: Keyword
quote =
  Keyword
    { symbol = "quote",
      handler = pure . Const . P.cadr
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
    collect (P.Pair (P.Atom (P.Symbol "unquote")) (P.Pair x P.Nil)) =
      (\p -> [(x, p)]) <$> objectify x
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
                Right (expanderProgram, _) ->
                  let newKeyword = Keyword {symbol = name, handler = invoke expanderProgram . P.cdr}
                   in do
                        modify (\r@GlobalPrepEnv {keywords = kw} -> r {keywords = (name, newKeyword) : kw})
                        return $ Const $ P.Atom (P.BoolLiteral True)
            _ -> throwError "Invalid macro name: expected a symbol"
        _ -> throwError "Invalid defmacro syntax: expected (defmacro (name <variables>) <body>)"
    }

invoke :: Program () () -> P.SExp -> ObjectifyM (Program () ())
invoke (Function vars body _) args =
  case P.listify args of
    Right argList
      | length argList == length vars -> do
          let action = local (extend $ zip vars argList) (evaluate (Sequence body))
          case runEvaluate action defaultRunTimeEnv of
            Left err -> throwError $ "Error in macro expansion: " ++ err
            Right res -> objectify res
      | otherwise ->
          throwError $
            "Macro invocation error: expected "
              ++ show (length vars)
              ++ " arguments, but got "
              ++ show (length (P.listify args))
    Left e -> throwError e
  where
    extend locals r@RunTimeEnv {rLocals} = r {rLocals = locals <> rLocals}
invoke _ _ = throwError "Internal error: expected a Function in macro invocation"

defaultPrepEnv :: (LocalPrepEnv, GlobalPrepEnv)
defaultPrepEnv =
  ( LocalPrepEnv [],
    GlobalPrepEnv
      { variables = [],
        keywords =
          map
            (\x -> (symbol x, x))
            [ if_,
              lambda,
              begin,
              set,
              quote,
              quasiquote,
              defmacro
            ]
      }
  )

defaultRunTimeEnv :: RunTimeEnv
defaultRunTimeEnv =
  RunTimeEnv
    { rLocals = [],
      rGlobals = []
    }

eval :: P.SExp -> IO ()
eval e = case runObjectify (objectify e) defaultPrepEnv of
  Left err -> print $ "comptime error: " ++ err
  Right (obj, _) -> case runEvaluate (evaluate obj) defaultRunTimeEnv of
    Left err -> print $ "runtime error: " ++ show err
    Right res -> print res
