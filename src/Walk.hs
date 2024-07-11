{-# LANGUAGE LambdaCase #-}

module Walk where

import Data.Foldable (find)
import Data.List (partition)
import MonadT
  ( Identity (..),
    MonadState (..),
    StateT (..),
  )
import Objectify (ParsedProgram, Program (..), Variable (..))

walk :: (Monad m) => ParsedProgram -> (ParsedProgram -> m (Program v f)) -> v -> f -> m (Program v f)
walk (Reference v) _ dv _ = pure $ Reference (v {vInfo = dv})
walk (Assignment v p) f dv _ = Assignment v {vInfo = dv} <$> f p
walk (Alternative pc pt pf) f _ _ = Alternative <$> f pc <*> f pt <*> f pf
walk (Sequence ps) f _ _ = Sequence <$> mapM f ps
walk p@(Function {vars, body}) f dv df =
  ( \x ->
      p
        { vars = map (\v -> v {vInfo = dv}) vars,
          body = x,
          fInfo = df
        }
  )
    <$> mapM f body
walk (Application func args) f _ _ = Application <$> f func <*> mapM f args
walk (IntLiteral i) _ _ _ = pure $ IntLiteral i
walk (BoolLiteral b) _ _ _ = pure $ BoolLiteral b
walk Nil _ _ _ = pure Nil
walk (Magic k) _ _ _ = pure $ Magic k
walk (Quote e) _ _ _ = pure $ Quote e
walk (QuasiQuote {}) _ _ _ = error "TODO"

newtype BoxVInfo = BoxVInfo {isMutable :: Bool}
  deriving (Show)

instance Show (Variable BoxVInfo) where
  show (Variable name False BoxVInfo {isMutable = False}) = name
  show (Variable name False BoxVInfo {isMutable = True}) = "*" ++ name
  show (Variable name True BoxVInfo {isMutable = False}) = "g:" ++ name
  show (Variable name True BoxVInfo {isMutable = True}) = "*g:" ++ name

newtype BoxFInfo = BoxFInfo {mutables :: [Variable BoxVInfo]}
  deriving (Show)

deriving instance Show (Program BoxVInfo BoxFInfo)

type BoxM = StateT [Variable BoxVInfo] Identity
-- ^               ^ variables of the current function

-- | Make read/write of local variables read/write in boxes.
box :: ParsedProgram -> Program BoxVInfo BoxFInfo
box p = fst . runIdentity $ runStateT (_box p) []

_box :: ParsedProgram -> BoxM (Program BoxVInfo BoxFInfo)
_box (Reference v) = do
  vars :: [Variable BoxVInfo] <- get
  case find ((== name v) . name) vars of
    Just modifiedVar -> return $ Reference modifiedVar
    Nothing -> error "unreachable"
_box (Assignment v f) =
  let mut = v {vInfo = BoxVInfo True}
   in modify (add mut) >> Assignment (v {vInfo = BoxVInfo True}) <$> _box f
  where
    add mut (x : xs)
      | name mut == name x = mut : xs
      | otherwise = x : add mut xs
    add _ [] = []
_box (Function {vars, body}) = do
  modify (map (\v -> v {vInfo = BoxVInfo False}) vars <>)
  let (assigns, pures) = partition (\case (_, Assignment _ _) -> True; (_, _) -> False) (zip [0 .. length body - 1] body)
  let (order, total) = unzip (assigns <> pures)
  bbody <- mapM _box total
  -- TODO: refactor
  let (assignsM, puresM) = splitAt (length assigns) (zip order bbody)
  let ordered = reorder assignsM puresM
  newVars <- take (length vars) <$> get
  modify (drop (length vars) :: [Variable BoxVInfo] -> [Variable BoxVInfo])
  return $ Function newVars ordered (BoxFInfo $ filter (isMutable . vInfo) newVars)
  where
    reorder xx@((i1, x) : xs) yy@((i2, y) : ys) =
      if i1 < i2
        then x : reorder xs yy
        else y : reorder xx ys
    reorder xx@(_ : _) [] = map snd xx
    reorder [] yy@(_ : _) = map snd yy
    reorder [] [] = []
_box p = walk p _box (BoxVInfo False) (BoxFInfo [])

-- type LiftM = StateT [LocalVariable] (ReaderT [LocalVariable] Identity)
-- -- ^                ^ Free variables         ^ Currently bounded variables
--
-- -- | Record free variables for lambda lifting
-- lift :: Program -> Program
-- lift p = fst . runIdentity $ runReaderT (runStateT (_lift p) []) []
--
-- _lift :: Program -> LiftM Program
-- _lift r@(LocalReference v@(LocalVariable {})) = do
--   bounded :: [LocalVariable] <- ask
--   if v `elem` bounded
--     then return r
--     else do
--       modify $ add v
--       return $ FreeReference v
--   where
--     add var free =
--       if var `notElem` free
--         then var : free
--         else free
-- _lift (Function vars body) = do
--   orig :: [LocalVariable] <- get
--   bbody <- local (const vars) (mapM _lift body)
--   free <- get
--   put orig
--   return $ FlatFunction vars bbody free
-- _lift p = walk p _lift
