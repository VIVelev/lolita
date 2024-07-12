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

-- | Keeps record of who (which local variable) is mutable.
newtype WhoIsMutable = WhoIsMutable
  { getWhoIsMutable :: [(Variable (), Bool)]
  }
  deriving (Show, Semigroup, Monoid)

type RecordMutableM = StateT WhoIsMutable Identity

deriving instance Show (Program () WhoIsMutable)

-- | Records which local variables introduced by a function are mutable.
recordMutable :: Program () () -> Program () WhoIsMutable
recordMutable p = fst . runIdentity $ runStateT (_recordMutable p) (WhoIsMutable [])

_recordMutable :: Program () () -> RecordMutableM (Program () WhoIsMutable)
_recordMutable (Reference v) = return $ Reference v
_recordMutable (Assignment v f) = do
  modify (mark v)
  Assignment v <$> _recordMutable f
  where
    mark var (WhoIsMutable (h@(vv, _) : vs))
      | name var == name vv = WhoIsMutable $ (vv, True) : vs
      | otherwise = WhoIsMutable $ h : getWhoIsMutable (mark var (WhoIsMutable vs))
    mark _ empty = empty
_recordMutable (Function {vars, body}) =
  let info0 = WhoIsMutable $ map (,False) vars
   in do
        modify (info0 <>)
        bbody <- mapM _recordMutable body
        info <- WhoIsMutable . take (length vars) . getWhoIsMutable <$> get
        modify (WhoIsMutable . drop (length vars) . getWhoIsMutable)
        return $ Function vars bbody info
_recordMutable p = walk p _recordMutable

-- | Indicates whether the variable is mutable or not
newtype IsMutable = IsMutable Bool

type MarkMutableM = ReaderT WhoIsMutable Identity

instance Show (Variable IsMutable) where
  show (Variable name False (IsMutable False)) = name
  show (Variable name False (IsMutable True)) = "*" ++ name
  show (Variable name True (IsMutable False)) = "g:" ++ name
  show (Variable name True (IsMutable True)) = "*g:" ++ name

deriving instance Show (Program IsMutable ())

-- | Once we know which variables are mutable (by running `recordMutable`)
-- we are ready to mark the mutable variables throughout the function body.
--
-- Note: Walking the tree twice is neccesary so we can correctly handle
-- cases such as
-- ```scheme
-- (lambda (x)
--   x
--   ((lambda () (set! x 1))))
-- ```
-- where `x` is, obviously, mutable.
markMutable :: Program () WhoIsMutable -> Program IsMutable ()
markMutable p = runIdentity $ runReaderT (_markMutable p) (WhoIsMutable [])

_markMutable :: Program () WhoIsMutable -> MarkMutableM (Program IsMutable ())
_markMutable (Reference v) = do
  info <- getWhoIsMutable <$> ask
  case lookup v info of
    Just b -> return $ Reference $ v {vInfo = IsMutable b}
    Nothing -> return $ Reference $ v {vInfo = IsMutable False}
-- \^ Note: setting IsMutable to True here essentially guesses that
-- the global variable is immutable. Can this be improved?
_markMutable (Assignment v f) = do
  info <- getWhoIsMutable <$> ask
  case lookup v info of
    Just b -> Assignment (v {vInfo = IsMutable b}) <$> _markMutable f
    Nothing -> Assignment (v {vInfo = IsMutable True}) <$> _markMutable f
_markMutable (Function _ body info) =
  let newVars = map (\(v, b) -> v {vInfo = IsMutable b}) (getWhoIsMutable info)
   in do
        bbody <- local (const info) (mapM _markMutable body)
        return $ Function newVars bbody ()
_markMutable p = walk p _markMutable

-- | Indicates whether the variable is free or not
newtype IsFree = IsFree Bool

-- | Collection of the free variables encountered in the body
-- of a function.
newtype FreeVars = FreeVars [Variable ()]
  deriving (Show)

type FreeM = StateT FreeVars (ReaderT [Variable ()] Identity)
-- ^                                  ^ bounded variables

instance Show (Variable IsFree) where
  show (Variable name False (IsFree False)) = name
  show (Variable name False (IsFree True)) = "~" ++ name
  show (Variable name True _) = "g:" ++ name

deriving instance Show (Program IsFree FreeVars)

-- | Collect and mark the free variables in a function.
free :: Program () () -> Program IsFree FreeVars
free p = fst . runIdentity $ runReaderT (runStateT (_free p) (FreeVars [])) []

_free :: Program () () -> FreeM (Program IsFree FreeVars)
_free (Reference v) = do
  bounded :: [Variable ()] <- ask
  case find ((== name v) . name) bounded of
    Just _ -> return $ Reference v {vInfo = IsFree False}
    Nothing -> do
      modify (add v)
      return $ Reference v {vInfo = IsFree True}
  where
    add var (FreeVars frees) = FreeVars $ var : frees
_free (Assignment v f) = do
  bounded :: [Variable ()] <- ask
  case find ((== name v) . name) bounded of
    Just _ -> Assignment v {vInfo = IsFree False} <$> _free f
    Nothing -> do
      modify (add v)
      Assignment v {vInfo = IsFree True} <$> _free f
  where
    add var (FreeVars vs) = FreeVars $ var : vs
_free (Function {vars, body}) = do
  FreeVars outerFree <- get
  outerBounded :: [Variable ()] <- ask
  bbody <-
    withState (const (FreeVars [])) $
      local (const vars) (mapM _free body)
  FreeVars innerFree <- get
  modify $ const (FreeVars (filter (`notIn` outerBounded) innerFree <> outerFree))
  return $
    Function
      (map (\v -> v {vInfo = IsFree False}) vars)
      bbody
      (FreeVars innerFree)
  where
    v `notIn` vs = all ((/= name v) . name) vs
_free p = walk p _free

-- instance Show (Variable MarkVInfo) where
--   show (Variable name False (MarkVInfo False False)) = name
--   show (Variable name False (MarkVInfo True False)) = "*" ++ name
--   show (Variable name False (MarkVInfo False True)) = "~" ++ name
--   show (Variable name False (MarkVInfo True True)) = "~*" ++ name
--   show (Variable name True (MarkVInfo False False)) = "g:" ++ name
--   show (Variable name True (MarkVInfo True False)) = "*g:" ++ name
--   show (Variable name True (MarkVInfo False True)) = "~g:" ++ name
--   show (Variable name True (MarkVInfo True True)) = "~*g:" ++ name
