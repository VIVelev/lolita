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
    ReaderT (..),
  )
import Parse qualified as P
  ( AKind (..),
    SExp (..),
    cadddr,
    caddr,
    cadr,
  )

data PrepEnv = PrepEnv
  { variables :: [(String, Variable)],
    keywords :: [(String, Keyword)]
  }

type Objectify = ErrorT String (ReaderT PrepEnv Identity)

runObjectify :: Objectify a -> PrepEnv -> Either String a
runObjectify obj env = runIdentity $ runReaderT (runErrorT obj) env

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
  | Function
      { vars :: [Variable],
        body :: Program
      }
  | -- | this is a special case
    Magic Keyword
  deriving (Show, Eq)

objectify :: P.SExp -> Objectify Program
objectify (P.Atom (P.IntLiteral i)) = return $ IntLiteral i
objectify (P.Atom (P.BoolLiteral b)) = return $ BoolLiteral b
objectify (P.Atom (P.Symbol name)) = do
  env <- ask
  case lookup name (variables env) of
    Just v@(LocalVariable {}) -> return $ LocalReference v
    Just v@(GlobalVariable _) -> return $ GlobalReference v
    Nothing -> case lookup name (keywords env) of
      Just k -> return $ Magic k
      Nothing ->
        let var = GlobalVariable name
            ref = GlobalReference var
         in local
              (\r@PrepEnv {variables} -> r {variables = (name, var) : variables})
              (pure ref)
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

-- | (lambda (<names> ...) body)
lambda :: Keyword
lambda =
  Keyword
    { symbol = "lambda",
      handler = \e ->
        let varsOrErr = variableList $ P.cadr e
            rawBody = P.caddr e
         in do
              case varsOrErr of
                Left err -> throwError err
                Right vars -> do
                  body <- local (extendEnv vars) (objectify rawBody)
                  return $ Function vars body
    }
  where
    variableList (P.Pair (P.Atom (P.Symbol name)) cdr) =
      (:) (LocalVariable name False False) <$> variableList cdr
    variableList P.Nil = Right []
    variableList _ = Left "lambda variables should only be symbols"
    extendEnv vars r@PrepEnv {variables} = r {variables = map (\x -> (name x, x)) vars <> variables}

defaultPrepEnv :: PrepEnv
defaultPrepEnv =
  PrepEnv
    { variables = [],
      keywords = map (\x -> (symbol x, x)) [if_, lambda]
    }
