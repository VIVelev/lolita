module MonadT where

import Control.Monad (ap)
import Data.Functor.Classes (Show1 (liftShowsPrec), showsPrec1)
import GHC.Base (liftM)

-- TODO: Could automatically derive Functor, Applicative
-- with `deriving ... via ...`

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

instance Show1 Identity where
  liftShowsPrec showsPrecA _ _ (Identity a) = showsPrecA 10 a -- Is 10 a hack?

instance (Show a) => Show (Identity a) where
  showsPrec = showsPrec1

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure = Identity
  (<*>) = ap

instance Monad Identity where
  return = pure
  (Identity a) >>= k = k a

-- | ErrorT Monad Transformer
-- Provides exception handling via `throwError` and `catchError`.
newtype ErrorT e m a = ErrorT {runErrorT :: m (Either e a)}

instance (Show e, Show1 m, Show a) => Show (ErrorT e m a) where
  show = show . runErrorT

instance (Monad m) => Functor (ErrorT e m) where
  fmap = liftM

instance (Monad m) => Applicative (ErrorT e m) where
  pure a = ErrorT . pure $ Right a
  (<*>) = ap

instance (Monad m) => Monad (ErrorT e m) where
  return = pure
  m >>= k = ErrorT $ do
    -- work on the inner monad
    x <- runErrorT m
    case x of
      Left e -> return (Left e)
      Right a -> runErrorT (k a)

-- | Monadic Error
-- `e` is the error type
-- `m` is the monad which implements error handling
class (Monad m) => MonadError e m where
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

instance (Monad m) => MonadError e (ErrorT e m) where
  throwError = ErrorT . return . Left
  m `catchError` h = ErrorT $ do
    -- work on the inner monad
    x <- runErrorT m
    case x of
      Left e -> runErrorT (h e)
      Right a -> return (Right a)

-- | ReaderT Monad Transformer
-- Provides an environment capabilities
-- `r` is the type of the environement
-- `m` is the type of the underlying monad
-- `a` is the type of the result
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Monad m) => Functor (ReaderT r m) where
  fmap = liftM

instance (Monad m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ \_ -> return a
  (<*>) = ap

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  m >>= k = ReaderT $ \r -> do
    -- work on the inner monad
    a <- runReaderT m r
    runReaderT (k a) r

instance (MonadError e m) => MonadError e (ReaderT r m) where
  throwError e = ReaderT $ \_ -> throwError e
  m `catchError` h = ReaderT $ \r ->
    runReaderT m r `catchError` \e -> runReaderT (h e) r

-- | Just return the environment itself
ask :: (Monad m) => ReaderT r m r
ask = ReaderT $ \r -> return r

-- | "Modify" the environment
-- the first argument is a function that takes in the environment
-- and returns a new one.
local :: (Monad m) => (r -> r) -> ReaderT r m a -> ReaderT r m a
local f m = ReaderT $ \r -> runReaderT m (f r)
