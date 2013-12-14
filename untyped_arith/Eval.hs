module Eval(RuntimeError, eval) where
import Syntax

type RuntimeError = String

instance Monad (Either e) where
  return = Right
  Right x >>= f = f x
  Left e >>= f = Left e

eval :: Term -> Either RuntimeError Value
eval TmTrue = return ValTrue
eval TmFalse = return ValFalse
eval (TmIf t1 t2 t3) = do
  v1 <- eval t1
  case v1 of
    ValTrue -> eval t2
    ValFalse -> eval t3
    other -> Left $ "Bad guard: " ++ show other
eval TmZero = return $ ValNv 0
eval (TmSucc t1) = do
  v1 <- eval t1
  case v1 of
    ValNv n -> return $ ValNv (n+1)
    other -> Left $ "Bad succ: " ++ show other
eval (TmPred t1) = do
  v1 <- eval t1
  case v1 of
    ValNv n | n > 0     -> return $ ValNv (n-1)
            | otherwise -> return $ ValNv 0
    other -> Left $ "Bad pred: " ++ show other
eval (TmIsZero t1) = do
  v1 <- eval t1
  case v1 of
    ValNv 0 -> return ValTrue
    ValNv n -> return ValFalse
    other -> Left $ "Bad iszero: " ++ show other
