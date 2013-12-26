{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Naming
( resolveTerm, resolveType
, renameTerm, renameType
) where
import qualified Data.List as L
import qualified Data.Maybe as M
import Control.Applicative
import qualified Control.Monad.Identity as I

import Syntax

resolveTerm :: TopCtx -> Term String -> Either String (Term NameBind)
resolveType :: TopCtx -> Type String -> Either String (Type NameBind)
resolveTerm = fst . resolve
resolveType = snd . resolve

resolve topCtx = (resTerm,resType) where
  resTerm = walkTerm [] bind use
  resType = walkType [] bind use

  bind vars x = x:vars
  use vars x
    | Just idx <- L.elemIndex x vars =
      Right $ LocalBind idx
    | Just idx <- L.findIndex ((== x) . fst) topCtx =
      Right $ TopBind idx
    | otherwise =
      Left $ "Variable `" ++ x ++ "` was not bound"
  
renameTerm :: TopCtx -> Term NameBind -> Term String
renameType :: TopCtx -> Type NameBind -> Type String
renameTerm = fst . rename
renameType = snd . rename

rename topCtx = (renTerm,renType) where
  renTerm = I.runIdentity . walkTerm [] bind use
  renType = I.runIdentity . walkType [] bind use

  bind vars x = if L.elem x vars || M.isJust (L.lookup x topCtx)
    then bind vars (x++"'")
    else x:vars
  use vars (LocalBind idx) = return $ vars !! idx
  use _    (TopBind idx) = return . fst $ topCtx !! idx



walkTerm :: Applicative m 
  => ctx
  -> (ctx -> String -> ctx)
  -> (ctx -> a -> m b)
  -> Term a
  -> m (Term b)
walkTerm ctx bind use = walk ctx where
  walkTy ctx = walkType ctx bind use
  walk ctx t = case t of
    TmVar a -> TmVar <$> use ctx a
    TmAbs x ty1 t2 -> TmAbs x <$> walkTy ctx ty1 <*> walk (bind ctx x) t2
    TmApp t1 t2 -> TmApp <$> walk ctx t1 <*> walk ctx t2
    TmTAbs x k1 t2 -> TmTAbs x k1 <$> walk (bind ctx x) t2
    TmTApp t1 ty2 -> TmTApp <$> walk ctx t1 <*> walkTy ctx ty2
    TmIf t1 t2 t3 -> TmIf <$> walk ctx t1 <*> walk ctx t2 <*> walk ctx t3
    TmTrue -> pure TmTrue
    TmFalse -> pure TmFalse
    TmUnit -> pure TmUnit

walkType :: Applicative m
  => ctx
  -> (ctx -> String -> ctx)
  -> (ctx -> a -> m b)
  -> Type a
  -> m (Type b)
walkType ctx bind use = walkTy ctx where
  walkTy ctx ty = case ty of
    TyVar a -> TyVar <$> use ctx a
    TyAbs x k1 ty2 -> TyAbs x k1 <$> walkTy (bind ctx x) ty2
    TyApp ty1 ty2 -> TyApp <$> walkTy ctx ty1 <*> walkTy ctx ty2
    TyAll x k1 ty2 -> TyAll x k1 <$> walkTy (bind ctx x) ty2
    TyArr ty1 ty2 -> TyArr <$> walkTy ctx ty1 <*> walkTy ctx ty2
    TyBool -> pure TyBool
    TyUnit -> pure TyUnit
