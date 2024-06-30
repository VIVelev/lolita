module Transformers where

import MonadT
import Data.Map qualified as Map
import Data.Maybe

type Name = String

data Exp
  = Lit Integer
  | Var Name
  | Add Exp Exp
  | Lam Name Exp
  | App Exp Exp
  deriving (Show)

data Value
  = IntVal Integer
  | FunVal Env Name Exp
  deriving (Show)

type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Add e1 e2) =
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
   in IntVal (i1 + i2)
eval0 env (Lam n e) = FunVal env n e
eval0 env (App e1 e2) =
  let val1 = eval0 env e1
      val2 = eval0 env e2
   in case val1 of
        FunVal env' n body -> eval0 (Map.insert n val2 env') body

exampleExp :: Exp
exampleExp = Lit 10 `Add` App (Lam "x" (Var "x")) (Lit 4 `Add` Lit 2)
errorExp :: Exp
errorExp = App (Lit 10) (Lit 12)

-- First Evaluator

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Add e1 e2) = do
  v1 <- eval1 env e1
  v2 <- eval1 env e2
  case (v1, v2) of
    (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
eval1 env (Lam n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
  v1 <- eval1 env e1
  v2 <- eval1 env e2
  case v1 of
    (FunVal e' n body) -> eval1 (Map.insert n v2 e') body


-- Second Evaluator: Exceptions

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 m = runIdentity $ runExceptT m

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = return $ fromJust $ Map.lookup n env
eval2 env (Add e1 e2) = do
  v1 <- eval2 env e1
  v2 <- eval2 env e2
  case (v1, v2) of
    (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
    _ -> throwExcept "type error"
eval2 env (Lam n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
  v1 <- eval2 env e1
  v2 <- eval2 env e2
  case v1 of
    (FunVal e' n body) -> eval2 (Map.insert n v2 e') body
    _ -> throwExcept "not a function"
