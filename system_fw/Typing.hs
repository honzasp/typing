--module Typing (typeOf, kindOf) where
module Typing where
import Control.Applicative
import Control.Monad
import qualified Data.Maybe as M

import Syntax

data VarBind
  = BindTermVar (Type NameBind)
  | BindTypeVar Kind
  deriving Show

typeOf :: TopCtx -> Term NameBind -> Either String (Type NameBind)
typeOf topCtx = typecheck topCtx []

kindOf :: TopCtx -> Type NameBind -> Either String Kind
kindOf topCtx = kindcheck topCtx []

typecheck :: TopCtx -> [VarBind] -> Term NameBind -> Either String (Type NameBind)
typecheck topCtx bnds t = check bnds t where
  check bnds t = case t of
    TmVar (LocalBind idx) -> case bnds !! idx of
      BindTermVar ty -> Right $ typeShift (idx+1) ty
      BindTypeVar _ -> Left $ "Type variable used in place of term"
    TmVar (TopBind idx) -> case snd $ topCtx !! idx of
      TopTermAbbr _ ty -> Right $ typeShiftTop (idx+1) ty
      TopTypeAbbr _ _ -> Left $ "Type abbreviation used in place of term"
    TmAbs _ ty1 t2 -> do
      k1 <- kindcheck topCtx bnds ty1 
      case k1 of
        KiStar -> Right ()
        KiArr _ _ -> Left $ "Term abstraction must be over proper type"
      ty2 <- typeShift (-1) <$> check (BindTermVar ty1:bnds) t2
      Right $ TyArr ty1 ty2
    TmApp t1 t2 -> do
      ty1 <- check bnds t1
      ty2 <- check bnds t2
      case typeWhnf topCtx ty1 of
        TyArr ty11 ty12 | typeEquiv topCtx ty11 ty2 -> Right ty12
        TyArr {} -> Left "Type of applied term does not match function"
        TyAll {} -> Left "Term was applied to forall type (missing type application?)"
        _        -> Left "Only functions may be applied"
    TmTAbs x k1 t2 ->
      TyAll x k1 <$> check (BindTypeVar k1:bnds) t2
    TmTApp t1 ty2 -> do
      k2 <- kindcheck topCtx bnds ty2 
      ty1 <- check bnds t1
      case typeWhnf topCtx ty1 of
        TyAll _ k11 ty12 | k11 == k2 ->
          Right $ typeApply ty2 ty12
        TyAll {} -> Left "Type application kind mismatch"
        _        -> Left "Only forall types may be [applied]"
    TmIf t1 t2 t3 -> do
      ty1 <- check bnds t1
      ty2 <- check bnds t2
      ty3 <- check bnds t3
      if typeEquiv topCtx TyBool ty1 then
        if typeEquiv topCtx ty2 ty3 then Right ty2
        else Left "Condition arms do not match"
      else Left "Condition guard must be Bool"
    TmAs t1 ty2 -> do
      ty1 <- check bnds t1
      if typeSub topCtx ty1 ty2
        then Right ty2
        else Left $ "Invalid upcast"
    TmRcd fs -> do
      tys <- mapM (check bnds) (map snd fs)
      Right . TyRcd $ zip (map fst fs) tys
    TmProj t1 f -> do
      ty1 <- check bnds t1
      case typeWhnf topCtx ty1 of
        TyRcd fs | Just ty2 <- lookup f fs -> Right ty2
        TyRcd{}  -> Left "Missing field projected"
        _        -> Left "Only records may be projected"
    TmTrue -> Right TyBool
    TmFalse -> Right TyBool
    TmUnit -> Right TyUnit

kindcheck :: TopCtx -> [VarBind] -> Type NameBind -> Either String Kind
kindcheck topCtx = check where
  check bnds ty = case ty of
    TyVar (LocalBind idx) -> case bnds !! idx of
      BindTypeVar k -> Right k
      BindTermVar _ -> Left $ "Term variable used in place of type"
    TyVar (TopBind idx) -> case snd $ topCtx !! idx of
      TopTypeAbbr ty k -> Right k
      TopTermAbbr _ _ -> Left $ "Term abbreviation used in place of type"
    TyAbs _ k1 ty2 ->
      KiArr k1 <$> check (BindTypeVar k1:bnds) ty2
    TyApp ty1 ty2 -> do
      k1 <- check bnds ty1
      k2 <- check bnds ty2
      case k1 of
        KiArr k11 k12 | k11 == k2 -> Right k12
        KiArr {} -> Left "Operator application kind mismatch"
        _        -> Left "Only arrow-kinded type operators may be applied"
    TyAll _ k1 ty2 -> do
      k2 <- check (BindTypeVar k1:bnds) ty2
      case k2 of
        KiStar -> Right KiStar
        _      -> Left "Universally quantified type must have star kind"
    TyArr ty1 ty2 -> do
      k1 <- check bnds ty1
      k2 <- check bnds ty2
      if k1 == KiStar && k2 == KiStar
        then Right KiStar
        else Left "Types in arrow must have star kind"
    TyRcd fs -> do
      ks <- mapM (check bnds . snd) fs
      if all (== KiStar) ks
        then Right KiStar
        else Left "All record types must have star kind"
    TyBool -> Right KiStar
    TyUnit -> Right KiStar

typeWhnf :: TopCtx -> Type NameBind -> Type NameBind
typeWhnf topCtx ty = case ty of
  TyVar (TopBind idx) | let TopTypeAbbr ty1 _ = snd $ topCtx !! idx ->
    typeShiftTop (-idx-1) $ typeWhnf topCtx (typeShiftTop (idx+1) ty1)
  TyApp ty1 ty2 | TyAbs _ k11 ty12 <- typeWhnf topCtx ty1 ->
    typeWhnf topCtx $ typeApply ty2 ty12
  ty -> ty

typeEquiv :: TopCtx -> Type NameBind -> Type NameBind -> Bool
typeEquiv topCtx = equiv where
  equiv ty1 ty2 = case (typeWhnf topCtx ty1,typeWhnf topCtx ty2) of
    (TyVar a,TyVar b) ->
      a == b
    (TyAbs _ k1 s1,TyAbs _ k2 s2) ->
      k1 == k2 && equiv s1 s2
    (TyApp s11 s12,TyApp s21 s22) ->
      equiv s11 s21 && equiv s12 s22
    (TyAll _ k1 s1,TyAll _ k2 s2) ->
      k1 == k2 && equiv s1 s2
    (TyArr s11 s12,TyArr s21 s22) ->
      equiv s11 s21 && equiv s12 s22
    (TyRcd fs1,TyRcd fs2) ->
      fieldsEquiv fs1 fs2
    (TyBool,TyBool) -> True
    (TyUnit,TyUnit) -> True
    (_,_) -> False

  fieldsEquiv [] [] = True
  fieldsEquiv ((f1,ty1):fs1) ((f2,ty2):fs2) =
    f1 == f2 && equiv ty1 ty2 && fieldsEquiv fs1 fs2
  fieldsEquiv _ _ = False

typeSub :: TopCtx -> Type NameBind -> Type NameBind -> Bool
typeSub topCtx = sub where
  sub ty1 ty2 = case (typeWhnf topCtx ty1,typeWhnf topCtx ty2) of
    (TyRcd fs1,TyRcd fs2) ->
      rcdSub fs1 fs2
    (TyArr ty11 ty12,TyArr ty21 ty22) ->
      sub ty21 ty11 && sub ty12 ty22
    (_,_) -> typeEquiv topCtx ty1 ty2

  rcdSub fs1 fs2 = all fieldSub fs2 where
    fieldSub (l,ty2) = M.isJust $ do
      ty1 <- lookup l fs1
      if sub ty1 ty2 then Just () else Nothing

typeApply :: Type NameBind -> Type NameBind -> Type NameBind
typeApply s bd = typeShift (-1) $ typeSubst 0 (typeShift 1 s) bd

typeSubst :: Int -> Type NameBind -> Type NameBind -> Type NameBind
typeSubst x s = typeMap onvar where
  onvar c (LocalBind k)
    | k == x + c = typeShift c s
  onvar c bind   = TyVar bind

typeShift :: Int -> Type NameBind -> Type NameBind
typeShift d = typeMap onvar where
  onvar c (LocalBind k)
    | k >= c   = TyVar $ LocalBind (k+d)
  onvar c bind = TyVar bind

typeShiftTop :: Int -> Type NameBind -> Type NameBind
typeShiftTop d = typeMap onvar where
  onvar c (TopBind k) = TyVar $ TopBind (k+d)
  onvar c bind = TyVar bind

typeMap :: (Int -> a -> Type b) -> Type a -> Type b
typeMap onvar = walk 0 where
  walk c ty = case ty of
    TyVar bnd -> onvar c bnd
    TyAbs x k ty1 -> TyAbs x k (walk (c+1) ty1)
    TyApp ty1 ty2 -> TyApp (walk c ty1) (walk c ty2)
    TyAll x k ty1 -> TyAll x k (walk (c+1) ty1)
    TyArr ty1 ty2 -> TyArr (walk c ty1) (walk c ty2)
    TyRcd fs -> TyRcd (map (walk c <$>) fs)
    TyBool -> TyBool
    TyUnit -> TyUnit
