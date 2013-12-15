module Eval(eval, shiftTerm) where
import Control.Monad.Instances()

import Context
import Term

mapTerm :: (Int -> Int -> Term) -> Term -> Term
mapTerm mapVar = walk 0 where
  walk :: Int -> Term -> Term
  walk c t = case t of
    TmVar k -> mapVar c k
    TmAbs x ty t1 -> TmAbs x ty $ walk (c+1) t1
    TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
    TmZero -> TmZero
    TmSucc t1 -> TmSucc (walk c t1)
    TmPred t1 -> TmPred (walk c t1)
    TmIszero t1 -> TmIszero (walk c t1)

substTerm :: Int -> Term -> Term -> Term
substTerm k s = mapTerm substVar where
  substVar c var = if c + k == var then shiftTerm c s else TmVar var

shiftTerm :: Int -> Term -> Term
shiftTerm d = mapTerm shiftVar where
  shiftVar c var = if var >= c then TmVar (var + d) else TmVar var

eval :: NameCtx -> Term -> Term
eval ctx (TmApp t1 t2)
  | TmAbs _ _ t12 <- v1 =
    eval ctx $ shiftTerm (-1) $ substTerm 0 (shiftTerm 1 v2) t12
  | otherwise = TmApp v1 v2
  where (v1,v2) = (eval ctx t1,eval ctx t2)
eval ctx (TmVar k)
  | (_,NBndTermBind t) <- ctxLookup k ctx = eval ctx $ shiftTerm (k+1) t
eval ctx (TmIf t1 t2 t3)
  | TmTrue <- v1  = v2
  | TmFalse <- v1 = v3
  | otherwise     = TmIf v1 t2 t3
  where (v1,v2,v3) = (eval ctx t1,eval ctx t2,eval ctx t3)
eval ctx (TmSucc t1) = TmSucc $ eval ctx t1
eval ctx (TmPred t1)
  | TmSucc nv <- v1, isNumericVal nv = nv
  | TmZero <- v1 = TmZero
  | otherwise    = TmPred v1
  where v1 = eval ctx t1
eval ctx (TmIszero t1)
  | TmSucc nv <- v1, isNumericVal nv = TmFalse
  | TmZero <- v1  = TmTrue
  | otherwise     = TmIszero v1
  where v1 = eval ctx t1
eval _ t = t

isNumericVal :: Term -> Bool
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal TmZero = True
isNumericVal _      = False
