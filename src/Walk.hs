{-# OPTIONS_GHC -Wno-orphans #-}

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

walk :: (Monad m) => Program v1 f1 -> (Program v1 f1 -> m (Program v2 f2)) -> m (Program v2 f2)
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

-- instance Show (Variable MarkVInfo) where
--   show (Variable name False (MarkVInfo False False)) = name
--   show (Variable name False (MarkVInfo True False)) = "*" ++ name
--   show (Variable name False (MarkVInfo False True)) = "~" ++ name
--   show (Variable name False (MarkVInfo True True)) = "~*" ++ name
--   show (Variable name True (MarkVInfo False False)) = "g:" ++ name
--   show (Variable name True (MarkVInfo True False)) = "*g:" ++ name
--   show (Variable name True (MarkVInfo False True)) = "~g:" ++ name
--   show (Variable name True (MarkVInfo True True)) = "~*g:" ++ name

-- | Records whether the functions variable is mutable.
type IsMutable = [(Variable (), Bool)]

type MutableM = StateT IsMutable Identity

deriving instance Show (Program () IsMutable)

-- | Marks whether the local variables introduced by a function
-- are mutable or not.
mutable :: Program () () -> Program () IsMutable
mutable p = fst . runIdentity $ runStateT (_mutable p) []

_mutable :: Program () () -> MutableM (Program () [(Variable (), Bool)])
_mutable (Reference v) = return $ Reference v
_mutable (Assignment v f) = do
  modify (markMutable v)
  Assignment v <$> _mutable f
  where
    markMutable var ((h@(vv, _) : vs) :: IsMutable)
      | name var == name vv = (vv, True) : vs
      | otherwise = h : markMutable var vs
    markMutable _ [] = []
_mutable (Function {vars, body}) =
  let info0 = map (,False) vars
   in do
        modify (info0 <>)
        bbody <- mapM _mutable body
        info <- take (length vars) <$> get
        modify (drop (length vars) :: IsMutable -> IsMutable)
        return $ Function vars bbody info
_mutable p = walk p _mutable

-- | Collection of the free variables encountered in the body
-- of a function.
type Free = [Variable ()]

type FreeM = StateT Free (ReaderT [Variable ()] Identity)
-- ^                              ^ bounded variables

deriving instance Show (Program () Free)

-- | Collect the free variables in a function.
free :: Program () () -> Program () Free
free p = fst . runIdentity $ runReaderT (runStateT (_free p) []) []

_free :: Program () () -> FreeM (Program () Free)
_free (Reference v) = do
  bounded :: [Variable ()] <- ask
  case find ((== name v) . name) bounded of
    Just _ -> pure ()
    Nothing -> modify (add v)
  return $ Reference v
  where
    add var frees = var : frees
_free (Assignment v f) = Assignment v <$> _free f
_free (Function {vars, body}) = do
  outerFree :: Free <- get
  outerBounded :: [Variable ()] <- ask
  bbody <-
    withState (const ([] :: Free)) $
      local (const vars) (mapM _free body)
  innerFree <- get
  modify $ const $ filter (notIn outerBounded) innerFree <> outerFree
  return $ Function vars bbody innerFree
  where
    notIn vs v = all ((/= name v) . name) vs
_free p = walk p _free
