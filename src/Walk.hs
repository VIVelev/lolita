module Walk where

import MonadT
  ( Identity (..),
    MonadReader (..),
    MonadState (..),
    ReaderT (..),
    StateT (..),
  )
import Objectify (LocalVariable (..), Program (..))

walk :: (Monad m) => Program -> (Program -> m Program) -> m Program
walk (LocalAssignment p1 p2) f = LocalAssignment <$> f p1 <*> f p2
walk (Alternative pc pt pf) f = Alternative <$> f pc <*> f pt <*> f pf
walk (Sequence ps) f = Sequence <$> mapM f ps
walk p@(Function {body}) f = (\x -> p {body = x}) <$> mapM f body
walk (Application func args) f = Application <$> f func <*> mapM f args
walk (BoxRead p) f = BoxRead <$> f p
walk (BoxWrite p1 p2) f = BoxWrite <$> f p1 <*> f p2
walk p _ = pure p

-- | Make read/write of local variables read/write in boxes.
box :: Program -> Program
box p = runIdentity $ _box p

type BoxM = Identity

_box :: Program -> BoxM Program
_box r@(LocalReference (LocalVariable {isMutable}))
  | isMutable = pure $ BoxRead r
  | otherwise = pure r
_box a@(LocalAssignment r@(LocalReference (LocalVariable {isMutable})) f)
  | isMutable = BoxWrite r <$> _box f
  | otherwise = pure a
_box f@(Function vars body) =
  let vs = boxify vars
      bs = mapM _box body
   in (\x -> f {body = vs <> x}) <$> bs
  where
    boxify (x@LocalVariable {isMutable} : xs)
      | isMutable = BoxCreate x : boxify xs
      | otherwise = boxify xs
    boxify [] = []
_box p = walk p _box

-- | Record free variables for lambda lifting
lift :: Program -> Program
lift p = fst . runIdentity $ runReaderT (runStateT (_lift p) []) []

type LiftM = StateT [LocalVariable] (ReaderT [LocalVariable] Identity)
-- ^                ^ Free variables         ^ Currently bounded variables

_lift :: Program -> LiftM Program
_lift r@(LocalReference v@(LocalVariable {})) = do
  bounded :: [LocalVariable] <- ask
  if v `elem` bounded
    then return r
    else do
      modify $ add v
      return $ FreeReference v
  where
    add var free =
      if var `notElem` free
        then var : free
        else free
_lift (Function vars body) = do
  orig :: [LocalVariable] <- get
  bbody <- local (const vars) (mapM _lift body)
  free <- get
  put orig
  return $ FlatFunction vars bbody free
_lift p = walk p _lift
