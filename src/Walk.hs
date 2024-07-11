module Walk where

import Data.Foldable (find)
import MonadT
  ( Identity (..),
    MonadReader (..),
    MonadState (..),
    ReaderT (..),
    StateT (..),
  )
import Objectify (Program (..), Variable (..))

walk :: (Monad m) => Program () () -> (Program () () -> m (Program v f)) -> m (Program v f)
walk (IntLiteral i) _ = pure $ IntLiteral i
walk (BoolLiteral b) _ = pure $ BoolLiteral b
walk Nil _ = pure Nil
walk (Reference _) _ = error "this should be implemented by the code walker"
walk (Assignment _ _) _ = error "this should be implemented by the code walker"
walk (Alternative pc pt pf) f = Alternative <$> f pc <*> f pt <*> f pf
walk (Sequence ps) f = Sequence <$> mapM f ps
walk (Function {}) _ = error "this should be implemented by the code walker"
walk (Application func args) f = Application <$> f func <*> mapM f args
walk (Magic k) _ = pure $ Magic k
walk (Quote e) _ = pure $ Quote e
walk (QuasiQuote {}) _ = error "TODO: what to do here?"

data MarkFInfo = MarkFInfo
  { -- | mutable variables in the body of the function
    mutable :: [Variable ()],
    -- | free variables in the body of the function
    free :: [Variable ()]
  }
  deriving (Show)

deriving instance Show (Program () MarkFInfo)

type MarkFM = StateT MarkFInfo (ReaderT [Variable ()] Identity)
-- ^                                    ^ Variables in the current local scope

-- | Mark mutable variables and free references in function definition
markf :: Program () () -> Program () MarkFInfo
markf p =
  fst . runIdentity $
    runReaderT
      (runStateT (_markf p) (MarkFInfo [] []))
      []

_markf :: Program () () -> MarkFM (Program () MarkFInfo)
_markf (Reference v) = do
  locals :: [Variable ()] <- ask
  case find ((== name v) . name) locals of
    Just _ -> pure ()
    Nothing -> modify (\i@MarkFInfo {free} -> i {free = v : free})
  return $ Reference v
_markf (Assignment v f) = do
  locals :: [Variable ()] <- ask
  case find ((== name v) . name) locals of
    Just _ -> pure ()
    Nothing -> modify (\i@MarkFInfo {mutable} -> i {mutable = v : mutable})
  Assignment v <$> _markf f
_markf (Function {vars, body}) = do
  body_ <- local (const vars) (mapM _markf body)
  Function vars body_ <$> get
_markf p = walk p _markf

-- instance Show (Variable MarkVInfo) where
--   show (Variable name False (MarkVInfo False False)) = name
--   show (Variable name False (MarkVInfo True False)) = "*" ++ name
--   show (Variable name False (MarkVInfo False True)) = "~" ++ name
--   show (Variable name False (MarkVInfo True True)) = "~*" ++ name
--   show (Variable name True (MarkVInfo False False)) = "g:" ++ name
--   show (Variable name True (MarkVInfo True False)) = "*g:" ++ name
--   show (Variable name True (MarkVInfo False True)) = "~g:" ++ name
--   show (Variable name True (MarkVInfo True True)) = "~*g:" ++ name
