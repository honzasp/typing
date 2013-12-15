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
    TmUnit -> TmUnit
    TmValue val -> TmValue (walkValue c val)

  walkValue :: Int -> Value -> Value
  walkValue c v = case v of
    ValAbs x ty t1 -> ValAbs x ty $ walk (c+1) t1
    ValTrue -> ValTrue
    ValFalse -> ValFalse
    ValNat n -> ValNat n
    ValUnit -> ValUnit

substTerm :: Int -> Term -> Term -> Term
substTerm k s = mapTerm substVar where
  substVar c var = if c + k == var then shiftTerm c s else TmVar var

shiftTerm :: Int -> Term -> Term
shiftTerm d = mapTerm shiftVar where
  shiftVar c var = if var >= c then TmVar (var + d) else TmVar var

eval :: NameCtx -> Term -> Value
eval ctx (TmVar k)
  | (_,NBndTermBind t) <- ctxLookup k ctx = eval ctx $ shiftTerm (k+1) t
eval ctx (TmAbs x ty t1) = ValAbs x ty t1
eval ctx (TmApp t1 t2)
  | ValAbs _ _ t12 <- v1 =
    eval ctx $ shiftTerm (-1) $ substTerm 0 (shiftTerm 1 (TmValue v2)) t12
  where (v1,v2) = (eval ctx t1,eval ctx t2)
eval ctx TmTrue = ValTrue
eval ctx TmFalse = ValFalse
eval ctx (TmIf t1 t2 t3)
  | ValTrue <- v1  = v2
  | ValFalse <- v1 = v3
  where (v1,v2,v3) = (eval ctx t1,eval ctx t2,eval ctx t3)
eval ctx TmZero = ValNat 0
eval ctx (TmSucc t1)
  | ValNat n <- v1 = ValNat (n+1)
  where v1 = eval ctx t1
eval ctx (TmPred t1)
  | ValNat n <- v1 = ValNat (max 0 (n-1))
  where v1 = eval ctx t1
eval ctx (TmIszero t1)
  | ValNat 0 <- v1 = ValTrue
  | ValNat n <- v1 = ValFalse
  where v1 = eval ctx t1
eval ctx (TmValue val) = val
eval ctx TmUnit = ValUnit
eval _ t = error "Evaluation got stuck"
