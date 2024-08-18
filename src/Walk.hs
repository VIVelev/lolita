{-# OPTIONS_GHC -Wno-orphans #-}

module Walk where

import Data.String (IsString (fromString))
import MonadT
  ( Identity (..),
    MonadReader (..),
    MonadState (..),
    ReaderT (..),
    StateT (..),
  )
import Objectify (Program (..), Variable (..))
import Parse qualified as P (SExp (..))

walk :: (Monad m) => Program v1 f1 -> (Program v1 f1 -> m (Program v2 f2)) -> m (Program v2 f2)
walk (Const e) _ = pure $ Const e
walk (Reference _) _ = error "this should be implemented by the code walker"
walk (Assignment _ _) _ = error "this should be implemented by the code walker"
walk (Alternative pc pt pf) f = Alternative <$> f pc <*> f pt <*> f pf
walk (Sequence ps) f = Sequence <$> mapM f ps
walk (Function {}) _ = error "this should be implemented by the code walker"
walk (Application func args) f = Application <$> f func <*> mapM f args
walk (Magic k) _ = pure $ Magic k
walk (QuasiQuote {}) _ = error "TODO: what to do here?"

data Primitive = Plus | Minus | Times | Eqn
  deriving (Eq)

instance IsString (Maybe Primitive) where
  fromString s = case s of
    "+" -> Just Plus
    "-" -> Just Minus
    "*" -> Just Times
    "=" -> Just Eqn
    _ -> Nothing

instance Show (Variable (Maybe Primitive)) where
  show v@(Variable {vInfo = isPrimitive}) =
    ( case isPrimitive of
        Just _ -> "@"
        Nothing -> ""
    )
      ++ show v {vInfo = ()}

type PrimitiveM = Identity

deriving instance Show (Program (Maybe Primitive) ())

primitive :: Program () () -> Program (Maybe Primitive) ()
primitive p = runIdentity (_primitive p)

_primitive :: Program () () -> PrimitiveM (Program (Maybe Primitive) ())
_primitive (Reference v@(Variable {name = name})) =
  return $ Reference (v {vInfo = fromString name})
_primitive (Assignment v@(Variable {name = name}) f) =
  return $
    Assignment
      v {vInfo = fromString name}
      (primitive f)
_primitive (Function vars body _) =
  let newVars = map (\v@(Variable {name = name}) -> v {vInfo = fromString name}) vars
   in return $ Function newVars (map primitive body) ()
_primitive p = walk p _primitive

-- | Keeps record of who (which local variable) is mutable.
newtype WhoIsMutable = WhoIsMutable
  { getWhoIsMutable :: [(Variable (Maybe Primitive), Bool)]
  }
  deriving (Show, Semigroup, Monoid)

type RecordMutableM = StateT WhoIsMutable Identity

deriving instance Show (Program (Maybe Primitive) WhoIsMutable)

-- | Records which local variables introduced by a function are mutable.
recordMutable :: Program (Maybe Primitive) () -> Program (Maybe Primitive) WhoIsMutable
recordMutable p = fst . runIdentity $ runStateT (_recordMutable p) (WhoIsMutable [])

_recordMutable ::
  Program (Maybe Primitive) () ->
  RecordMutableM (Program (Maybe Primitive) WhoIsMutable)
_recordMutable (Reference v) = return $ Reference v
_recordMutable (Assignment v f) = do
  modify (mark v)
  Assignment v <$> _recordMutable f
  where
    mark var (WhoIsMutable (h@(vv, _) : vs))
      | name var == name vv = WhoIsMutable $ (vv, True) : vs
      | otherwise = WhoIsMutable $ h : getWhoIsMutable (mark var (WhoIsMutable vs))
    mark _ empty = empty
_recordMutable (Function {Objectify.vars, Objectify.body}) =
  let info0 = WhoIsMutable $ map (,False) vars
   in do
        modify (info0 <>)
        bbody <- mapM _recordMutable body
        info <- WhoIsMutable . take (length vars) . getWhoIsMutable <$> get
        modify (WhoIsMutable . drop (length vars) . getWhoIsMutable)
        return $ Function vars bbody info
_recordMutable p = walk p _recordMutable

-- | Indicates whether the variable is mutable or not
data IsMutablePrimitive = IsMutablePrimitive Bool (Maybe Primitive)

type MarkMutableM = ReaderT WhoIsMutable Identity

instance Show (Variable IsMutablePrimitive) where
  show v@(Variable {vInfo = IsMutablePrimitive False isPrimitive}) = show v {vInfo = isPrimitive}
  show v@(Variable {vInfo = IsMutablePrimitive True isPrimitive}) = "*" ++ show v {vInfo = isPrimitive}

deriving instance Show (Program IsMutablePrimitive ())

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
markMutable :: Program (Maybe Primitive) WhoIsMutable -> Program IsMutablePrimitive ()
markMutable p = runIdentity $ runReaderT (_markMutable p) (WhoIsMutable [])

lookupByName :: Variable v1 -> [(Variable v2, a)] -> Maybe a
lookupByName v@(Variable {name = n1}) ((Variable {name = n2}, r) : xs)
  | n1 == n2 = Just r
  | otherwise = lookupByName v xs
lookupByName _ [] = Nothing

toIsMutablePrimitive :: Bool -> Variable (Maybe Primitive) -> Variable IsMutablePrimitive
toIsMutablePrimitive isMutable v@(Variable {vInfo = isPrimitive}) =
  v {vInfo = IsMutablePrimitive isMutable isPrimitive}

_markMutable ::
  Program (Maybe Primitive) WhoIsMutable ->
  MarkMutableM (Program IsMutablePrimitive ())
_markMutable (Reference v) = do
  info <- getWhoIsMutable <$> ask
  case lookupByName v info of
    Just b -> return $ Reference $ toIsMutablePrimitive b v
    Nothing -> return $ Reference $ toIsMutablePrimitive False v
-- \^ Note: setting IsMutable to True here essentially guesses that
-- the global variable is immutable. This should be improved!
_markMutable (Assignment v@(Variable {vInfo = isPrimitive}) f) = do
  Assignment (v {vInfo = IsMutablePrimitive True isPrimitive}) <$> _markMutable f
_markMutable (Function _ body info) =
  let newVars = map (\(v, b) -> toIsMutablePrimitive b v) (getWhoIsMutable info)
   in do
        bbody <- local (const info) (mapM _markMutable body)
        return $ Function newVars bbody ()
_markMutable p = walk p _markMutable

-- | Indicates whether the variable is free/mutable.
data IsFreeMutablePrimitive = IsFreeMutablePrimitive Bool Bool (Maybe Primitive)
  deriving (Eq)

-- | Collection of the free variables encountered in the body
-- of a function.
newtype FreeVars = FreeVars
  { getFreeVars :: [Variable IsFreeMutablePrimitive]
  }
  deriving (Eq, Semigroup, Monoid)

instance Show FreeVars where
  show (FreeVars vs) = "FreeVars " ++ show vs

type FreeM = StateT FreeVars (ReaderT [Variable IsMutablePrimitive] Identity)
-- ^                                  ^ bounded variables

instance Show (Variable IsFreeMutablePrimitive) where
  show v@(Variable {vInfo = IsFreeMutablePrimitive False isMutable isPrimitive}) =
    show v {vInfo = IsMutablePrimitive isMutable isPrimitive}
  show v@(Variable {vInfo = IsFreeMutablePrimitive True isMutable isPrimitive}) =
    "~" ++ show v {vInfo = IsMutablePrimitive isMutable isPrimitive}

deriving instance Show (Program IsFreeMutablePrimitive FreeVars)

toIsFreeMutablePrimitive :: Bool -> Variable IsMutablePrimitive -> Variable IsFreeMutablePrimitive
toIsFreeMutablePrimitive isFree v@(Variable {vInfo = IsMutablePrimitive isMutable isPrimitive}) =
  v {vInfo = IsFreeMutablePrimitive isFree isMutable isPrimitive}

-- | Collect and mark the free variables in a function.
--
-- Note: This stage could be performed together with `markMutable`,
-- however I am going to keep them separate for simplicity. Also,
-- performing `markMutable` and then `free` is an arbitrary choice.
-- Swapping the order won't change anything.
free :: Program IsMutablePrimitive () -> Program IsFreeMutablePrimitive FreeVars
free p = fst . runIdentity $ runReaderT (runStateT (_free p) (FreeVars [])) []

notIn :: Variable v1 -> [Variable v2] -> Bool
v `notIn` vs = all ((/= name v) . name) vs

_free :: Program IsMutablePrimitive () -> FreeM (Program IsFreeMutablePrimitive FreeVars)
_free (Reference v) = do
  bounded :: [Variable IsMutablePrimitive] <- ask
  if v `notIn` bounded
    then do
      let newVar = toIsFreeMutablePrimitive True v
      modify (\vs -> if newVar `notIn` getFreeVars vs then vs <> FreeVars [newVar] else vs)
      return $ Reference newVar
    else return $ Reference $ toIsFreeMutablePrimitive False v
_free (Assignment v f) = do
  bounded :: [Variable IsMutablePrimitive] <- ask
  if v `notIn` bounded
    then do
      let newVar = toIsFreeMutablePrimitive True v
      modify (\vs -> if newVar `notIn` getFreeVars vs then vs <> FreeVars [newVar] else vs)
      Assignment newVar <$> _free f
    else Assignment (toIsFreeMutablePrimitive False v) <$> _free f
_free (Function {Objectify.vars, Objectify.body}) = do
  outerFree :: FreeVars <- get
  body' <-
    put (FreeVars [])
      >> local (const vars) (mapM _free body)
  innerFree :: FreeVars <- get
  outerBounded :: [Variable IsMutablePrimitive] <- ask
  put $ outerFree <> (FreeVars . filter (`notIn` outerBounded) . getFreeVars $ innerFree)
  return $
    Function
      (map (toIsFreeMutablePrimitive False) vars)
      body'
      innerFree
_free p = walk p _free

type IsFreeMutablePrimitiveOrQuote = Either P.SExp IsFreeMutablePrimitive

instance Show (Variable IsFreeMutablePrimitiveOrQuote) where
  show v@Variable {vInfo = Right i} = show $ v {vInfo = i}
  show Variable {name, vInfo = Left _} = "#" ++ name

fromIsFreeMutable ::
  Variable IsFreeMutablePrimitive ->
  Variable IsFreeMutablePrimitiveOrQuote
fromIsFreeMutable v@Variable {vInfo} = v {vInfo = Right vInfo}

data IndexFreeVars = IndexFreeVars Int [Variable IsFreeMutablePrimitiveOrQuote]
  deriving (Show)

deriving instance Show (Program IsFreeMutablePrimitiveOrQuote IndexFreeVars)

data FunctionDefinition = FunctionDefinition
  { vars :: [Variable IsFreeMutablePrimitiveOrQuote],
    body :: [Program IsFreeMutablePrimitiveOrQuote IndexFreeVars],
    freeVars :: [Variable IsFreeMutablePrimitiveOrQuote],
    index :: Int
  }
  deriving (Show)

data Quotation = Quotation Int P.SExp
  deriving (Show)

data FlattenedProgram = FlattenedProgram
  { form :: Program IsFreeMutablePrimitiveOrQuote IndexFreeVars,
    definitions :: [FunctionDefinition],
    quotations :: [Quotation]
  }
  deriving (Show)

type ExtractM = StateT ([FunctionDefinition], [Quotation]) Identity

-- | Extract quotations and function definitions
extract :: Program IsFreeMutablePrimitive FreeVars -> FlattenedProgram
extract p =
  let (form, (definitions, quotations)) = (runIdentity $ runStateT (_extract p) ([], []))
      index = length definitions
      definitions' = FunctionDefinition [] [form] [] index : definitions
      form' = Application (Function [] [] (IndexFreeVars index [])) []
   in FlattenedProgram form' definitions' quotations

_extract ::
  Program IsFreeMutablePrimitive FreeVars ->
  ExtractM (Program IsFreeMutablePrimitiveOrQuote IndexFreeVars)
_extract (Const c) = do
  (ds :: [FunctionDefinition], qs) <- get
  let index = length qs
  let q = Quotation index c
  put (ds, q : qs)
  return $ Reference $ Variable (show index) False (Left c)
_extract (Reference v) = pure $ Reference (fromIsFreeMutable v)
_extract (Assignment v f) = Assignment (fromIsFreeMutable v) <$> _extract f
_extract (Function vars body (FreeVars fvars)) =
  let vars' = map fromIsFreeMutable vars
      fvars' = map fromIsFreeMutable fvars
   in do
        body' <- mapM _extract body
        (ds, qs :: [Quotation]) <- get
        let index = length ds
        let def = FunctionDefinition vars' body' fvars' index
        put (def : ds, qs)
        return $ Function vars' [] (IndexFreeVars index fvars')
_extract p = walk p _extract
