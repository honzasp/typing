module Eval(evaluate) where
import Control.Applicative
import qualified Data.List as L

import Syntax

evaluate :: TopCtx -> Term NameBind -> Value NameBind
evaluate topCtx = evalCtx topCtx []

evalCtx :: TopCtx -> [Value NameBind] -> Term NameBind -> Value NameBind
evalCtx topCtx = eval where
  eval env t = case t of
    TmVar (LocalBind idx) -> env !! idx
    TmVar (TopBind idx) ->
      let (_,TopTermAbbr t _):topCtx' = drop idx topCtx
      in  evaluate topCtx' t
    TmAbs x _ t2 -> ValAbs x t2 env topCtx
    TmApp t1 t2 ->
      let ValAbs _ t12 env1 topCtx' = eval env t1
      in  evalCtx topCtx' (eval env t2:env1) t12
    TmTAbs x _ t2 -> ValTAbs x t2 env topCtx
    TmTApp t1 _ -> 
      let ValTAbs _ t12 env1 topCtx' = eval env t1
      in  evalCtx topCtx' (ValDummyType:env1) t12
    TmIf t1 t2 t3 ->
      let ValBool cond = eval env t1
      in  if cond then eval env t2 else eval env t3
    TmAs t1 ty2 -> eval env t1
    TmRcd fs -> ValRcd $ map (eval env <$>) fs
    TmProj t1 f -> 
      let ValRcd fs = eval env t1
          Just v2 = lookup f fs
      in v2
    TmVariant l t1 -> ValVariant l $ eval env t1
    TmCase t1 alts ->
      let ValVariant l v11 = eval env t1
          Just (_,_,t2) = L.find (\(l',_,_) -> l' == l) alts
      in  eval (v11:env) t2
    TmTrue -> ValBool True
    TmFalse -> ValBool False
    TmUnit -> ValUnit
