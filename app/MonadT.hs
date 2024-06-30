module MonadT where

import Control.Monad (ap)
import GHC.Base (liftM)
import Data.Functor.Classes (Show1 (liftShowsPrec), showsPrec1)

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
  liftShowsPrec showsPrecA _ p (Identity a) = 
    showString "Identity " . showParen (p > 10) (showsPrecA 11 a)

instance Show a => Show (Identity a) where
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

-- NOTE: Why is Monad constraint necessary?
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
