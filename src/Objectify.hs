{-# LANGUAGE LambdaCase #-}

-- | Turn S-expression into object tree (AST?)
-- that can be walked.
module Objectify where

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
import Text.Printf (printf)

type Error = String

data PrepEnv = PrepEnv
  { pLocals :: [(String, LocalVariable)],
    pGlobals :: [(String, GlobalVariable)],
    keywords :: [(String, Keyword)]
  }

type Objectify = ErrorT String (StateT PrepEnv Identity)

runObjectify :: Objectify a -> PrepEnv -> Either Error a
runObjectify obj env = fst <$> runIdentity $ runStateT (runErrorT obj) env

data RunTimeEnv = RunTimeEnv
  { rLocals :: [(LocalVariable, P.SExp)],
    rGlobals :: [(GlobalVariable, P.SExp)]
  }

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

data LocalVariable = LocalVariable
  { name :: String,
    isMutable :: Bool,
    isDotted :: Bool
  }

instance Eq LocalVariable where
  v1 == v2 = name v1 == name v2

instance Show LocalVariable where
  show (LocalVariable name False False) = name
  show (LocalVariable name True False) = printf "*%s" name
  show (LocalVariable name False True) = printf "[%s]" name
  show (LocalVariable name True True) = printf "*[%s]" name

newtype GlobalVariable = GlobalVariable String
  deriving (Eq)

instance Show GlobalVariable where
  show (GlobalVariable name) = "g:" ++ name

data Program
  = IntLiteral Integer
  | BoolLiteral Bool
  | Nil
  | LocalReference LocalVariable
  | GlobalReference GlobalVariable
  | LocalAssignment LocalVariable Program
  | GlobalAssignment GlobalVariable Program
  | Alternative
      { condition :: Program,
        consequent :: Program,
        alternate :: Program
      }
  | Sequence [Program]
  | Function
      { vars :: [LocalVariable],
        body :: [Program]
      }
  | Application
      { func :: Program,
        args :: [Program]
      }
  | -- | This is a special case, as you may guess from the name.
    Magic Keyword
  | -- | Quoting is vital for a language to support macros.
    Quote P.SExp
  | QuasiQuote
      { root :: P.SExp,
        unquotes :: [(P.SExp, Program)]
      }
  | -- | The following are results of a walker.
    BoxRead LocalVariable
  | BoxWrite LocalVariable Program
  | BoxCreate LocalVariable
  deriving (Show, Eq)

evaluate :: Program -> Evaluate P.SExp
evaluate (IntLiteral i) = return $ P.Atom (P.IntLiteral i)
evaluate (BoolLiteral b) = return $ P.Atom (P.BoolLiteral b)
evaluate Nil = return P.Nil
evaluate (LocalReference var) = do
  env <- ask
  case lookup var (rLocals env) of
    Just e -> return e
    Nothing -> throwError "unbound local variable"
evaluate (GlobalReference var) = do
  env <- ask
  case lookup var (rGlobals env) of
    Just e -> return e
    Nothing -> throwError "unbound local variable"
evaluate (Alternative ec et ef) = do
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
  case lookup name (pLocals env) of
    Just v@(LocalVariable {}) -> return $ LocalReference v
    Nothing -> case lookup name (pGlobals env) of
      Just v@(GlobalVariable _) -> return $ GlobalReference v
      Nothing -> case lookup name (keywords env) of
        Just k -> return $ Magic k
        Nothing ->
          let var = GlobalVariable name
              ref = GlobalReference var
           in do
                modify (\r@PrepEnv {pGlobals} -> r {pGlobals = (name, var) : pGlobals})
                return ref
objectify P.Nil = return Nil
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
objectifyWithScope :: P.SExp -> P.SExp -> Objectify Program
objectifyWithScope varsExp bodyExp =
  case parseVars varsExp of
    Right vars ->
      case P.listify bodyExp of
        Right es -> do
          modify (extend vars)
          body <- mapM objectify es
          locals <- map snd . pLocals <$> get
          modify (restore vars)
          return $ Function (take (length vars) locals) body
        Left err -> throwError $ "in body: " ++ show err
    Left err -> throwError $ "in variable list: " ++ show err
  where
    parseVars = \case
      P.Nil -> Right []
      P.Pair (P.Atom (P.Symbol name)) rest ->
        Right (LocalVariable name False False :) <*> parseVars rest
      _ -> Left "Invalid argument list, only symbols are allowed as lambda variables"
    extend
      vars
      r@PrepEnv {pLocals} =
        r {pLocals = zip (map name vars) vars <> pLocals}
    restore
      vars
      r@PrepEnv {pLocals} =
        let size = length vars
         in r {pLocals = drop size pLocals}

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
              LocalReference var ->
                let mutVar = var {isMutable = True}
                 in do
                      modify $ inLocals (replace (name var) mutVar)
                      return $ LocalAssignment mutVar form
              GlobalReference var -> return $ GlobalAssignment var form
              _ -> throwError $ "Cannot set: " ++ show n
        _ -> throwError "Invalid set! syntax"
    }
  where
    replace _ _ [] = []
    replace key newValue ((k, v) : xs)
      | k == key = (k, newValue) : xs
      | otherwise = (k, v) : replace key newValue xs
    inLocals f p@PrepEnv {pLocals} = p {pLocals = f pLocals}

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

defaultPrepEnv :: PrepEnv
defaultPrepEnv =
  PrepEnv
    { pLocals = [],
      pGlobals = [],
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

defaultRunTimeEnv :: RunTimeEnv
defaultRunTimeEnv =
  RunTimeEnv
    { rLocals = [],
      rGlobals = []
    }

eval :: P.SExp -> IO ()
eval e = case runObjectify (objectify e) defaultPrepEnv of
  Left err -> print $ "comptime errro: " ++ err
  Right obj -> case runEvaluate (evaluate obj) defaultRunTimeEnv of
    Left err -> print $ "runtime error: " ++ show err
    Right res -> print res
