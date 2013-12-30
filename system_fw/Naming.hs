{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Naming
( resolveTerm, resolveType, resolveValue
, renameTerm, renameType, renameValue
) where
import qualified Data.List as L
import qualified Data.Maybe as M
import Control.Applicative
import qualified Control.Monad.Identity as I

import Syntax

resolveTerm :: TopCtx -> Term String -> Either String (Term NameBind)
resolveType :: TopCtx -> Type String -> Either String (Type NameBind)
resolveValue :: TopCtx -> Value String -> Either String (Value NameBind)
resolveTerm = resolve walkTerm
resolveType = resolve walkType
resolveValue = resolve walkValue

resolve walk topCtx = walk [] bind use where
  bind vars x = (x:vars,x)
  use vars x
    | Just idx <- L.elemIndex x vars =
      Right $ LocalBind idx
    | Just idx <- L.findIndex ((== x) . fst) topCtx =
      Right $ TopBind idx
    | otherwise =
      Left $ "Variable `" ++ x ++ "` was not bound"
  
renameTerm :: TopCtx -> Term NameBind -> Term String
renameType :: TopCtx -> Type NameBind -> Type String
renameValue :: TopCtx -> Value NameBind -> Value String
renameTerm = rename walkTerm
renameType = rename walkType
renameValue = rename walkValue

rename walk topCtx = I.runIdentity . walk [] bind use where
  bind vars x = if L.elem x vars || M.isJust (L.lookup x topCtx)
    then bind vars (x++"'")
    else (x:vars,x)
  use vars (LocalBind idx) = return $ vars !! idx
  use _    (TopBind idx) = return . fst $ topCtx !! idx


walkTerm :: Applicative m
  => ctx
  -> (ctx -> String -> (ctx,String))
  -> (ctx -> a -> m b)
  -> Term a
  -> m (Term b)
walkTerm ctx bind use = walk ctx where
  walkTy ctx = walkType ctx bind use
  walk ctx t = case t of
    TmVar a -> TmVar <$> use ctx a
    TmAbs x ty1 t2 -> TmAbs x' <$> walkTy ctx ty1 <*> walk ctx' t2
      where (ctx',x') = bind ctx x
    TmApp t1 t2 -> TmApp <$> walk ctx t1 <*> walk ctx t2
    TmTAbs x k1 t2 -> TmTAbs x' k1 <$> walk ctx' t2
      where (ctx',x') = bind ctx x
    TmTApp t1 ty2 -> TmTApp <$> walk ctx t1 <*> walkTy ctx ty2
    TmIf t1 t2 t3 -> TmIf <$> walk ctx t1 <*> walk ctx t2 <*> walk ctx t3
    TmAs t1 ty2 -> TmAs <$> walk ctx t1 <*> walkTy ctx ty2
    TmTrue -> pure TmTrue
    TmFalse -> pure TmFalse
    TmUnit -> pure TmUnit

walkType :: Applicative m
  => ctx
  -> (ctx -> String -> (ctx,String))
  -> (ctx -> a -> m b)
  -> Type a
  -> m (Type b)
walkType ctx bind use = walkTy ctx where
  walkTy ctx ty = case ty of
    TyVar a -> TyVar <$> use ctx a
    TyAbs x k1 ty2 -> TyAbs x' k1 <$> walkTy ctx' ty2
      where (ctx',x') = bind ctx x
    TyApp ty1 ty2 -> TyApp <$> walkTy ctx ty1 <*> walkTy ctx ty2
    TyAll x k1 ty2 -> TyAll x' k1 <$> walkTy ctx' ty2
      where (ctx',x') = bind ctx x
    TyArr ty1 ty2 -> TyArr <$> walkTy ctx ty1 <*> walkTy ctx ty2
    TyBool -> pure TyBool
    TyUnit -> pure TyUnit

walkValue :: (Applicative m, Monad m)
  => ctx
  -> (ctx -> String -> (ctx,String))
  -> (ctx -> a -> m b)
  -> Value a 
  -> m (Value b)
walkValue ctx bind use = walk ctx where
  walkTe ctx = walkTerm ctx bind use
  walk ctx v = case v of
    ValAbs x t2 env -> ValAbs x' <$> walkTe ctx t2 <*> mapM (walk ctx') env
      where (ctx',x') = bind ctx x
    ValTAbs x t2 env -> ValTAbs x' <$> walkTe ctx t2 <*> mapM (walk ctx') env
      where (ctx',x') = bind ctx x
    ValBool b -> pure $ ValBool b
    ValUnit -> pure $ ValUnit
