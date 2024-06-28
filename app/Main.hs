{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Main where

import Prelude hiding (lookup)

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
  | At Position Term

data Value
  = Wrong
  | Num Int
  | Fun (Value -> P Value)

instance Show Value where
  show Wrong = "<wrong>"
  show (Num i) = show i
  show (Fun _) = "<function>"

type Environment = [(Name, Value)]

interp :: Term -> Environment -> P Value
interp (Var x) e = lookup x e
interp (Con i) _ = unitP (Num i)
interp (Add u v) e =
  interp u e `bindP` \a ->
    interp v e `bindP` \b ->
      add a b
interp (Lam x t) e = unitP (Fun (\a -> interp t ((x, a) : e)))
interp (App t u) e =
  interp t e `bindP` \f ->
    interp u e `bindP` \a ->
      apply f a
interp (At q t) e = resetP q $ interp t e

lookup :: Name -> Environment -> P Value
lookup x [] = errorP $ "unknown variable: " ++ x
lookup x ((y, b) : e) = if x == y then unitP b else lookup x e

add :: Value -> Value -> P Value
add (Num i) (Num j) = unitP $ Num (i + j)
add a b =
  errorP $
    "shoud be numbers: "
      ++ show a
      ++ ","
      ++ show b

apply :: Value -> Value -> P Value
apply (Fun f) a = f a
apply t _ = errorP $ "not a function: " ++ show t

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

{- ORMOLU_DISABLE -}
-- Suppose the input file is:
-- ((lambda (x)
--    (+ x x))
--   (+ 10 11))
--
term :: Term
term =
  At (Position (0, 0))
     (App (At (Position (0, 1))
              (Lam "x"
                (At (Position (1, 3))
                    (Add
                      (At (Position (1, 6)) (Var "x"))
                      (At (Position (1, 8)) (Var "y"))))))
          (At (Position (2, 2))
              (Add
                (At (Position (2, 5)) (Con 10))
                (At (Position (2, 8)) (Con 11)))))
{- ORMOLU_ENABLE -}

test :: Term -> String
test t = showP $ interp t []

main :: IO ()
main = print $ test term
