module Eval where
import Syntax

shiftTerm :: Int -> Term -> Term
shiftTerm d = shift 0 where
  shift c t = case t of
    TmVar k | k < c     -> TmVar k
            | otherwise -> TmVar (k+d)
    TmAbs x t1 -> TmAbs x $ shift (c+1) t1
    TmApp t1 t2 -> TmApp (shift c t1) (shift c t2)

substTerm :: Int -> Term -> Term -> Term
substTerm j s t = case t of
  TmVar k | k == j    -> s
          | otherwise -> TmVar k
  TmAbs x t1 -> TmAbs x $ substTerm (j+1) (shiftTerm 1 s) t1
  TmApp t1 t2 -> TmApp (substTerm j s t1) (substTerm j s t2)

eval :: Context -> Term -> Term
eval ctx (TmApp t1 t2)
  | TmAbs x t12 <- v1 = eval ctx $ shiftTerm (-1) $ substTerm 0 (shiftTerm 1 v2) t12
  where (v1,v2) = (eval ctx t1,eval ctx t2)
eval ctx (TmVar k)
  | TermBind t <- ctxLookup k ctx = eval ctx $ shiftTerm (k+1) t
eval ctx t = t
