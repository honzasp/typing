module Eval(eval) where
import Control.Monad.Instances()

import Context
import Term

mapTerm :: (Int -> Int -> Term) -> Term -> Term
mapTerm mapVar = walk 0 where
  walk :: Int -> Term -> Term
  walk c t = case t of
    TmVar k -> mapVar c k
    TmAbs x ty t1 -> TmAbs x ty (walk (c+1) t1)
    TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
    TmZero -> TmZero
    TmSucc t1 -> TmSucc (walk c t1)
    TmPred t1 -> TmPred (walk c t1)
    TmIszero t1 -> TmIszero (walk c t1)
    TmUnit -> TmUnit
    TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c+1) t2)
    TmTuple ts -> TmTuple (map (walk c) ts)
    TmProj t1 i -> TmProj (walk c t1) i
    TmFix t1 -> TmFix (walk c t1)
    TmValue val -> TmValue (walkValue c val)

  walkValue :: Int -> Value -> Value
  walkValue c v = case v of
    ValAbs x ty t1 -> ValAbs x ty (walk (c+1) t1)
    ValTrue -> ValTrue
    ValFalse -> ValFalse
    ValNat n -> ValNat n
    ValUnit -> ValUnit
    ValTuple vs -> ValTuple (map (walkValue c) vs)

substTerm :: Int -> Term -> Term -> Term
substTerm k s = mapTerm substVar where
  substVar c var = if c + k == var then shiftTerm c s else TmVar var

shiftTerm :: Int -> Term -> Term
shiftTerm d = mapTerm shiftVar where
  shiftVar c var = if var >= c then TmVar (var + d) else TmVar var

eval :: NameCtx -> Term -> Value
eval ctx t = case t of
  TmVar k
    | (_,NBndTermBind t) <- ctxLookup k ctx -> eval ctx $ shiftTerm (k+1) t
  TmAbs x ty t1 -> ValAbs x ty t1
  TmApp t1 t2
    | ValAbs _ _ t12 <- v1 -> eval ctx $ apply (TmValue v2) t12
    where (v1,v2) = (eval ctx t1,eval ctx t2)
  TmTrue -> ValTrue
  TmFalse -> ValFalse
  TmIf t1 t2 t3
    | ValTrue <- v1  -> v2
    | ValFalse <- v1 -> v3
    where (v1,v2,v3) = (eval ctx t1,eval ctx t2,eval ctx t3)
  TmZero -> ValNat 0
  TmSucc t1
    | ValNat n <- v1 -> ValNat (n+1)
    where v1 = eval ctx t1
  TmPred t1
    | ValNat n <- v1 -> ValNat (max 0 (n-1))
    where v1 = eval ctx t1
  TmIszero t1
    | ValNat 0 <- v1 -> ValTrue
    | ValNat n <- v1 -> ValFalse
    where v1 = eval ctx t1
  TmUnit -> ValUnit
  TmLet x t1 t2 -> eval ctx $ apply (TmValue v1) t2
    where v1 = eval ctx t1
  TmTuple ts -> ValTuple $ map (eval ctx) ts
  TmProj t1 i
    | ValTuple vs <- v1 -> vs !! (i-1)
    where v1 = eval ctx t1
  TmFix t1
    | ValAbs _ _ t2 <- v1 -> eval ctx $ apply t t2
    where v1 = eval ctx t1
  TmValue val -> val
  t -> error "Evaluation got stuck"
  where apply arg body = shiftTerm (-1) $ substTerm 0 (shiftTerm 1 arg) body
