module Eval where

import Data.Map
import MonadT (Identity, ReaderT(..), runIdentity)
import Parse (SExp (..))

type Environment = Map String SExp

type Eval = ReaderT Environment Identity
runEval :: Eval a -> Environment -> a
runEval e env = runIdentity $ runReaderT e env

data MagicKeyword a = MagicKeyword
  { symbol :: String,
    handler :: SExp -> Environment -> SExp
  }

eval :: SExp -> Eval SExp
eval (Atom s) = return (Atom s)
