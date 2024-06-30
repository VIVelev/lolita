module MonadT where

import Control.Monad (ap)
import Data.Functor.Classes (Show1 (liftShowsPrec), showsPrec1)
import GHC.Base (liftM)
import GHC.IO (catchException)

-- TODO: Could automatically derive Functor, Applicative
-- with `deriving ... via ...`

-- Some notes:
--
-- What is the default implementation of `liftM`?
--
-- > liftM f m = do
-- >   a <- m
-- >   return (f a)
--
-- What about `ap`?
--
-- > mf `ap` ma = do
-- >   f <- mf
-- >   a <- ma
-- >   return (f a)

-- | Identity Monad
--   Used at the bottom of the Monad Tower
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

-- | ExceptT Monad Transformer
--   Provides exception handling via `throwExcept` and `catchExcept`.
newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}

instance (Show e, Show1 m, Show a) => Show (ExceptT e m a) where
  show = show . runExceptT

instance (Monad m) => Functor (ExceptT e m) where
  fmap = liftM

instance (Monad m) => Applicative (ExceptT e m) where
  pure a = ExceptT . pure $ Right a
  (<*>) = ap

instance (Monad m) => Monad (ExceptT e m) where
  return = pure
  m >>= k = ExceptT $ do
    x <- runExceptT m
    case x of
      Left e -> return (Left e)
      Right a -> runExceptT (k a)

throwExcept :: (Monad m) => e -> ExceptT e m a
throwExcept = ExceptT . return . Left

catchExcept :: (Monad m) => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
m `catchExcept` h = ExceptT $ do
  -- think of `h` as the error handler
  x <- runExceptT m
  case x of
    Left e -> runExceptT (h e)
    Right a -> return (Right a)
