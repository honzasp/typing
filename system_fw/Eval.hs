module Eval(evaluate) where
import Control.Applicative

import Syntax

evaluate :: TopCtx -> Term NameBind -> Value NameBind
evaluate topCtx = eval [] where
  eval env t = case t of
    TmVar (LocalBind idx) -> env !! idx
    TmVar (TopBind idx) ->
      let (_,TopTermAbbr t _):topCtx' = drop idx topCtx
      in  evaluate topCtx' t
    TmAbs x _ t2 -> ValAbs x t2 env
    TmApp t1 t2 ->
      let ValAbs _ t12 env1 = eval env t1
      in  eval (eval env t2:env1) t12
    TmTAbs x _ t2 -> ValTAbs x t2 env
    TmTApp t1 _ -> 
      let ValTAbs _ t12 env1 = eval env t1
      in  eval (error "Dummy pseudo-type value used":env1) t12
    TmIf t1 t2 t3 ->
      let ValBool cond = eval env t1
      in  if cond then eval env t2 else eval env t3
    TmAs t1 ty2 -> eval env t1
    TmRcd fs -> ValRcd $ map (eval env <$>) fs
    TmProj t1 f -> 
      let ValRcd fs = eval env t1
          Just v2 = lookup f fs
      in v2
    TmTrue -> ValBool True
    TmFalse -> ValBool False
    TmUnit -> ValUnit
