module Typing(typeOf) where
import Control.Applicative((<$>))

import Context
import Term

typeOf :: TypeCtx -> Term -> Either String Type
typeOf ctx t = case t of
  TmVar k ->
    let TBndVarBind ty = ctxLookup k ctx in Right ty
  TmAbs _ ty1 t1 ->
    TyArr ty1 <$> typeOf (ctxBind (TBndVarBind ty1) ctx) t1
  TmApp t1 t2 -> do
    ty1 <- typeOf ctx t1
    ty2 <- typeOf ctx t2
    case ty1 of
      TyArr ty11 ty12 | ty11 == ty2 -> Right ty12
      TyArr ty11 ty12 -> Left $ "Wrong function type"
      other -> Left $ "Not a function type"
  TmTrue -> Right $ TyBool
  TmFalse -> Right $ TyBool
  TmIf t1 t2 t3 -> do
    ty1 <- typeOf ctx t1
    ty2 <- typeOf ctx t2
    ty3 <- typeOf ctx t3
    case ty1 of
      TyBool | ty2 == ty3 -> Right $ ty2
      TyBool -> Left $ "If arms do not match"
      _      -> Left $ "If condition not a bool"
  TmZero -> Right $ TyNat
  TmSucc t1 ->
    typeOf ctx t1 >>= expectTy "Succ arg must be Nat" TyNat >> return TyNat
  TmPred t1 -> do
    typeOf ctx t1 >>= expectTy "Pred arg must be Nat" TyNat >> return TyNat
  TmIszero t1 -> 
    typeOf ctx t1 >>= expectTy "Iszero arg must be Nat" TyNat >> return TyBool
  TmUnit -> Right $ TyUnit
  TmLet _ t1 t2 -> do
    ty1 <- typeOf ctx t1
    typeOf (ctxBind (TBndVarBind ty1) ctx) t2
  TmValue val ->
    error "Typechecked value"
  where
    expectTy :: String -> Type -> Type -> Either String ()
    expectTy msg t1 t2 = if t1 == t2 then Right () else Left msg
