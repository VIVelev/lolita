-- | Minimal implementation of mtl style Monad Transformers
-- Why? Cuz we need to understand stuff from the ground!
--
-- References:
--   https://dl.acm.org/doi/pdf/10.1145/143165.143169
--   https://blogs.asarkar.com/assets/docs/haskell/Monad%20Transformers%20Step%20by%20Step%20-%20Grabmuller.pdf
--   https://github.com/sdiehl/wiwinwlh
--   https://github.com/hmemcpy/milewski-ctfp-pdf
module MonadT where

import Control.Monad (ap)
import GHC.Base (liftM)

-- Some notes:
--
-- What is the default implementation of `liftM`?
--
-- >>> liftM f m = do
-- >>>   a <- m
-- >>>   return (f a)
--
-- What about `ap`?
--
-- >>> mf `ap` ma = do
-- >>>   f <- mf
-- >>>   a <- ma
-- >>>   return (f a)

-- | Identity Monad
-- Used at the bottom of the Monad Tower
newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure = Identity
  (<*>) = ap

instance Monad Identity where
  return = pure
  (Identity a) >>= k = k a

-- | Error Monad Transformer
-- Provides exception handling via `throwError` and `catchError`.
newtype ErrorT e m a = ErrorT {runErrorT :: m (Either e a)}

instance (Monad m) => Functor (ErrorT e m) where
  fmap = liftM

instance (Monad m) => Applicative (ErrorT e m) where
  pure a = ErrorT . pure $ Right a
  (<*>) = ap

instance (Monad m) => Monad (ErrorT e m) where
  return = pure
  m >>= k = ErrorT $ do
    x <- runErrorT m
    case x of
      Left e -> return (Left e)
      Right a -> runErrorT (k a)

-- | Monadic Error
-- `e` being the error type
class (Show e, Monad m) => MonadError e m where
  -- | Throw an Error
  -- `a` is usually a type that can hold the error
  -- nested inside, like `Either`.
  throwError :: e -> m a

  -- | Catch an Error
  -- The lhs is the monad so far in the computation, the rhs
  -- is the exception handler `h` that takes in the error
  -- type `e` and returns a monad.
  --
  -- Note the similarity between `catchError` and `>>=`. They are *almost*
  -- the same with the exception that the lhs acts on the error and
  -- rather then producing a new monad container (i.e. `m b`) it returns
  -- the same one.
  catchError :: m a -> (e -> m a) -> m a

instance (Show e, Monad m) => MonadError e (ErrorT e m) where
  throwError = ErrorT . return . Left
  m `catchError` h = ErrorT $ do
    x <- runErrorT m
    case x of
      Left e -> runErrorT (h e)
      Right a -> return (Right a)

-- | Reader Monad Transformer
-- Provides an environment capabilities
-- `r` is the type of the environement
-- `m` is the type of the underlying monad
-- `a` is the type of the result
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Monad m) => Functor (ReaderT r m) where
  fmap = liftM

instance (Monad m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure . const a)
  (<*>) = ap

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  m >>= k = ReaderT $ \r -> do
    a <- runReaderT m r
    runReaderT (k a) r

instance (MonadError e m) => MonadError e (ReaderT r m) where
  throwError e = ReaderT (const $ throwError e)
  m `catchError` h = ReaderT $ \r ->
    runReaderT m r `catchError` \e -> runReaderT (h e) r

-- | Monadic Reader
-- `r` being the type of the object to be read from; typically
-- some sort of an environment like `Map String Value`.
class (Monad m) => MonadReader r m where
  -- | Retrieves the monad environment.
  ask :: m r

  -- | Executes a computation in a modified environment.
  local :: (r -> r) -> m a -> m a

instance (Monad m) => MonadReader r (ReaderT r m) where
  ask = ReaderT return
  local f m = ReaderT (runReaderT m . f)

instance (MonadReader r m) => MonadReader r (ErrorT e m) where
  ask = ErrorT (Right <$> ask)
  local f m = ErrorT (local f $ runErrorT m)

-- | State Monad Transformer
-- Provides a read/write state of type `s`
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Monad m) => Functor (StateT s m) where
  fmap = liftM

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ pure . (a,)
  (<*>) = ap

instance (Monad m) => Monad (StateT s m) where
  return = pure
  m >>= k = StateT $ \s -> do
    (a, s') <- runStateT m s
    (b, s'') <- runStateT (k a) s'
    return (b, s'')

-- | Monadic State
-- `s` being the state type
class (Monad m) => MonadState s m where
  -- | Just return the state
  get :: m s

  -- | Replace the state
  put :: s -> m ()

  -- | Maps an old state to a new state inside a state monad.
  -- The old state is thrown away.
  modify :: (s -> s) -> m ()
  modify f = get >>= put . f

  withState :: (s -> s) -> m a -> m a
  withState f m = modify f >> m

  -- | Executes a computation in a modified state. Once
  -- the computation is done, the state is restored.
  -- TODO: maybe this is not needed after all
  locally :: (s -> s) -> m a -> m a
  locally f m = do
    original <- get
    put (f original)
    res <- m
    put original
    return res

instance (Monad m) => MonadState s (StateT s m) where
  get = StateT $ \s -> pure (s, s)
  put s = StateT $ \_ -> pure ((), s)

  -- \| Direct implementation of locally.
  locally f m = StateT $ \s -> do
    (a, _) <- runStateT m (f s)
    return (a, s)

instance (Show e, MonadState s m) => MonadState s (ErrorT e m) where
  get = ErrorT $ Right <$> get
  put s = ErrorT $ Right <$> put s

-- TODO: Instances
--  - [ ] ReaderT of MonadState
--  - [ ] StateT of MonadReader
--  - [ ] StateT of MonadError
