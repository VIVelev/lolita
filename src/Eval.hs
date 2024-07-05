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
  ( AKind (..),
    SExp (..),
    cadddr,
    caddr,
    cadr,
    cddr,
    listify,
  )

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
  show = show . symbol

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
  | GlobalReference Variable
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
  | -- | this is a special case
    Magic Keyword
  | Quote P.SExp
  deriving (Show, Eq)

evaluate :: Program -> Evaluate P.SExp
evaluate (Quote e) = return e
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
evaluate (Altarnative ec et ef) =
  let c = evaluate ec
   in do
        b <- c
        case b of
          (P.Atom (P.BoolLiteral True)) -> evaluate et
          (P.Atom (P.BoolLiteral False)) -> evaluate ef
          _ -> throwError "value not boolifyable"
evaluate (Sequence p) = foldl1 (>>) (map evaluate p)
evaluate _ = throwError "not yet implemented"

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
      handler = \e ->
        let condition = objectify $ P.cadr e
            consequent = objectify $ P.caddr e
            alternate = objectify $ P.cadddr e
         in Altarnative <$> condition <*> consequent <*> alternate
    }

-- | Objectifies a sequence of expressions
-- as if they are executed sequentially.
--
-- It returns a Sequence.
objectifySequence :: P.SExp -> Objectify Program
objectifySequence e =
  let es = P.listify e
   in Sequence . reverse
        <$> foldl
          (\b a -> (:) <$> ((head <$> b) >> objectify a) <*> b)
          ((:) <$> objectify (head es) <*> pure [])
          (tail es)

-- | Given s-expressions `(v1 v2 v3 ...)` and `(e1 e2 e3 ...)`
-- it returns the program `(begin e1 e2 e3)`, so that the program
-- referes to the variables v1, v2, etc.
--
-- It returns a Function.
objectifyWithScope :: P.SExp -> P.SExp -> Objectify Program
objectifyWithScope rawVars rawBody =
  let varsOrErr = variableList rawVars
   in do
        case varsOrErr of
          Left err -> throwError err
          Right vars -> do
            body <- localState (extendEnv vars) (objectifySequence rawBody)
            return $ Function vars body
  where
    variableList (P.Pair (P.Atom (P.Symbol name)) cdr) =
      (:) (LocalVariable name False False) <$> variableList cdr
    variableList P.Nil = Right []
    variableList _ = Left "only symbols are allowed as lambda variables"
    extendEnv vars r@PrepEnv {variables} = r {variables = map (\x -> (name x, x)) vars <> variables}

-- | (lambda (<names> ...) body)
lambda :: Keyword
lambda =
  Keyword
    { symbol = "lambda",
      handler = \e ->
        let vars = P.cadr e
            body = P.cddr e
         in objectifyWithScope vars body
    }

-- | (begin (begin ...)
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

-- | (defmacro (name <variables>)
--     <body>)
defmacro :: Keyword
defmacro =
  Keyword
    { symbol = "defmacro",
      handler = \e ->
        let call = P.cadr e
            P.Atom (P.Symbol name) = P.car call
            vars = P.cdr call
            body = P.cddr e
            Right expanderProgam = runObjectify (objectifyWithScope vars body) defaultPrepEnv
            -- \^ this is a Function Program
            -- Now, the handler invokes this function with the arguments supplied
            newKeyword = Keyword {symbol = name, handler = objectify . invoke expanderProgam . P.cdr}
            defmacroReturnValue = BoolLiteral True -- the expression `(defmacro ...)` has to return something
         in do
              modify (\r@PrepEnv {keywords} -> r {keywords = (name, newKeyword) : keywords})
              return defmacroReturnValue
    }
  where
    invoke (Function vars body) (args :: P.SExp) =
      let Right res =
            runEvaluate
              ( local
                  (\r -> zip vars (P.listify args) ++ r)
                  (evaluate body)
              )
              defaultRunTimeEnv
       in res

defaultPrepEnv :: PrepEnv
defaultPrepEnv =
  PrepEnv
    { variables = [],
      keywords =
        map
          (\x -> (symbol x, x))
          [ if_,
            lambda,
            quote,
            begin,
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
