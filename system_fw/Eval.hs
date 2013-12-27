module Eval(evaluate) where
import Syntax

evaluate :: TopCtx -> Term NameBind -> Value NameBind
evaluate topCtx = eval [] where
  eval env t = case t of
    TmVar (LocalBind idx) -> env !! idx
    TmVar (TopBind idx) ->
      let (_,TopTermAbbr t _):topCtx' = drop idx topCtx
      in  evaluate topCtx' t
    TmAbs x _ t2 -> ValLambda x t2 env
    TmApp t1 t2 ->
      let (v1,v2) = (eval env t1,eval env t2)
          ValLambda _ t12 env = v1
      in  eval (v2:env) t12
    TmTAbs _ _ t2 -> eval env t2
    TmTApp t1 _ -> eval env t1
    TmIf t1 t2 t3 ->
      let ValBool cond = eval env t1
      in  if cond then eval env t2 else eval env t3
    TmTrue -> ValBool True
    TmFalse -> ValBool False
    TmUnit -> ValUnit
