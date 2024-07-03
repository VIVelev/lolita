{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Prelude hiding (lookup)
import Control.Monad (liftM)
import GHC.Base (ap)

type Name = String

newtype Position = Position (Integer, Integer)

instance Show Position where
  show (Position (row, col)) = "row: " ++ show row ++ ", col:" ++ show col

pos0 :: Position
pos0 = Position (0, 0)

data Term
  = Var Name
  | Con Int
  | Add Term Term
  | Lam Name Term
  | App Term Term
  -- | At Position Term
  | CallCC Name Term

data Value
  = Wrong String
  | Num Int
  | Fun (Value -> K Value)

instance Show Value where
  show (Wrong s) = "<wrong>: " ++ s
  show (Num i) = show i
  show (Fun _) = "<function>"

type Environment = [(Name, Value)]

interp :: Term -> Environment -> K Value
interp (Var x) e = lookup x e
interp (Con i) _ = return (Num i)
interp (Add u v) e = do
  a <- interp u e
  b <- interp v e
  return $ add a b
interp (Lam x t) e = return $ Fun (\a -> interp t ((x, a) : e))
interp (App t u) e = do
  f <- interp t e
  a <- interp u e
  case f of
    (Fun ff) -> ff a
    _ -> return $ Wrong "not a function"
-- interp (At _ t) e = interp t e
interp (CallCC x t) e =
  K $ \c -> runK (interp t $ (x, Fun $ \v -> K $ \_ -> c v) : e) c

lookup :: Name -> Environment -> K Value
lookup _ [] = return $ Wrong "lookup failed"
lookup x ((y, b) : e) = if x == y then return b else lookup x e

add :: Value -> Value -> Value
add (Num i) (Num j) = Num $ i + j
add _ _ = Wrong "not a number"

-- Error Monad
data E a = Success a | Error String

unitE :: a -> E a
unitE = Success

errorE :: String -> E a
errorE = Error

bindE :: E a -> (a -> E b) -> E b
(Success a) `bindE` k = k a
(Error s) `bindE` _ = Error s

showE :: (Show a) => E a -> String
showE (Success a) = "Success: " ++ show a
showE (Error s) = "Error: " ++ s

-- Position Monad
type P a = Position -> E a

unitP :: a -> P a
unitP a _ = unitE a

errorP :: (Show a) => String -> P a
errorP s p = errorE $ show p ++ ": " ++ show s

bindP :: P a -> (a -> P b) -> P b
m `bindP` k = \p -> m p `bindE` \a -> k a p

showP :: (Show a) => P a -> String
showP m = showE (m pos0)

resetP :: Position -> P a -> P a
resetP q m = \_ -> m q

-- State Monad
type State = Integer

type S a = State -> (a, State)

unitS :: a -> S a
unitS a = \s0 -> (a, s0)

bindS :: S a -> (a -> S b) -> S b
m `bindS` k = \s0 ->
  let (a, s1) = m s0
      (b, s2) = k a s1
   in (b, s2)

showS :: (Show a) => S a -> String
showS m =
  let (a, s0) = m 0
   in "Result: " ++ show a ++ "; Count: " ++ show s0

tickS :: S ()
tickS s0 = ((), s0 + 1)

-- Continuation
type Answer = Value
newtype K a = K { runK :: (a -> Answer) -> Answer }

instance Functor K where
  fmap = liftM

instance Applicative K where
  pure = return
  (<*>) = ap

instance Monad K where
  m >>= k = K $ \c -> runK m $ \a -> runK (k a) c
  return a = K $ \c -> c a

instance Show (K Value) where
  show m = show $ runK m id

-- Suppose the input file is:
-- ((lambda (x)
--    (+ x x))
--   (+ 10 11))
--
term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 10) (Con 11))

-- Suppose the input file is:
-- ((lambda (x)
--    (+ x x))
--   (+ 10 11))
--
-- The above code is the same as the following code:
-- ((lambda (x)
--    (+ x x))
--   21)
--
-- The above code is the same as the following code:
-- (+ 21 21)


-- Now with Position matadata
-- term :: Term
-- term =
--   At (Position (0, 0))
--      (App (At (Position (0, 1))
--               (Lam "x"
--                 (At (Position (1, 3))
--                     (Add
--                       (At (Position (1, 6)) (Var "x"))
--                       (At (Position (1, 8)) (Var "x"))))))
--           (At (Position (2, 2))
--               (Add
--                 (At (Position (2, 5)) (Con 10))
--                 (At (Position (2, 8)) (Con 11)))))

-- A call/cc!
-- Suppose we have the following code:
-- ((lambda (x)
--    (call/cc (k) (+ x (k x))))
--   (+ 10 11))
-- This translates to:
term2 :: Term
term2 =
  App
    (Lam "x" (CallCC "k" (Add (Var "x") (App (Var "k") (Var "x")))))
    (Add (Con 10) (Con 11))

test :: Term -> String
test t = show $ interp t []

main :: IO ()
main = print $ test term2
