module Typing (typeOf) where
import Control.Applicative((<$>))

import Syntax

typeOf :: TopCtx -> Term -> Either String Type
typeOf topCtx = (typeHNF <$>) . check ([],[]) where
  check :: ([Type],[Kind]) -> Term -> Either String Type
  check ctx@(teVs,tyVs) t = case t of
    TmVar k -> Right $ teVs !! k
    TmAbs _ ty1 t2 -> do
      k1 <- kindCheck tyVs ty1 
      case k1 of
        KiStar    -> Right ()
        KiArr _ _ -> Left "Abstraction argument must have kind *"
      ty2 <- check (ty1:teVs,tyVs) t2
      Right $ TyArr ty1 ty2
    TmApp t1 t2 -> do
      ty1 <- typeWHNF <$> check ctx t1
      ty2 <- check ctx t2
      case ty1 of
        TyArr ty11 ty12 | typeEquiv ty11 ty2 -> Right ty12
        --TyArr _ _ -> Left "Wrong argument type"
        TyArr _ _ -> Left $ "Wrong argument type: " ++ show (ty1,ty2) ++ " | " ++ show (t1,t2)
        _         -> Left "Function must be of arrow type"
    TmTAbs x k1 t2 -> do
      ty2 <- check (teVs,k1:tyVs) t2
      Right $ TyAll x k1 ty2
    TmTApp t1 ty2 -> do
      ty1 <- typeWHNF <$> check ctx t1
      k2 <- kindCheck tyVs ty2
      case ty1 of
        TyAll _ k11 ty12 | k11 == k2 -> Right $ typeSubst0 ty2 ty12
        TyAll _ _ _ -> Left "Kinds of type applications must match"
        _ -> Left "Type application works only on universally quantified types"
    TmTrue -> Right TyBool
    TmFalse -> Right TyBool
    TmIf t1 t2 t3 -> do
      ty1 <- typeWHNF <$> check ctx t1
      ty2 <- check ctx t2
      ty3 <- check ctx t3
      case ty1 of
        TyBool | typeEquiv ty2 ty3 -> Right ty2
        TyBool -> Left "Condition arms do not match"
        _      -> Left "Condition guard must be Bool"

  typeWHNF :: Type -> Type
  typeWHNF ty = case ty of
    TyApp ty1 ty2 
      | TyAbs _ k11 s12 <- typeWHNF ty1 -> typeSubst0 (typeWHNF ty2) s12
    ty -> ty

  kindCheck :: [Kind] -> Type -> Either String Kind
  kindCheck tyVs ty = case ty of
    TyVar k -> Right $ tyVs !! k
    TyAll _ k1 ty2 -> do
      k2 <- kindCheck (k1:tyVs) ty2
      case k2 of
        KiStar -> Right KiStar
        KiArr _ _ -> Left "Only proper types can be universally quantified"
    TyAbs _ k1 ty2 -> do
      k2 <- kindCheck (k1:tyVs) ty2
      Right $ KiArr k1 k2
    TyApp ty1 ty2 -> do
      k1 <- kindCheck tyVs ty1
      k2 <- kindCheck tyVs ty2
      case k1 of
        KiArr k11 k12 | k11 == k2 -> Right k12
        KiArr _ _ -> Left "Kinds of type operator arguments must match"
        _ -> Left "Only types with arrow kinds can act as type operators"
    TyArr ty1 ty2 -> do
      k1 <- kindCheck tyVs ty1
      k2 <- kindCheck tyVs ty2
      case (k1,k2) of
        (KiStar,KiStar) -> Right KiStar
        _ -> Left "Arrow type operands must be proper types"
    TyBool -> Right KiStar

  typeEquiv :: Type -> Type -> Bool
  typeEquiv ty1 ty2 = case (typeWHNF ty1,typeWHNF ty2) of
    (TyVar k,TyVar l) -> k == l
    (TyAll _ k1 ty11,TyAll _ k2 ty21) ->
      k1 == k2 && typeEquiv ty11 ty21
    (TyAbs _ k1 ty11,TyAbs _ k2 ty21) ->
      k1 == k2 && typeEquiv ty11 ty21
    (TyApp ty11 ty12,TyApp ty21 ty22) ->
      typeEquiv ty11 ty21 && typeEquiv ty12 ty22
    (TyArr ty11 ty12,TyArr ty21 ty22) ->
      typeEquiv ty11 ty21 && typeEquiv ty12 ty22
    (TyBool,TyBool) -> True
    (_,_) -> False

  typeHNF :: Type -> Type
  typeHNF ty = case ty of
    TyVar k -> TyVar k
    TyAll x k ty1 -> TyAll x k (typeHNF ty1)
    TyAbs x k ty1 -> TyAbs x k (typeHNF ty1)
    TyApp ty1 ty2
      | TyAbs _ k1 ty12 <- ty1' -> typeSubst0 ty2' ty12
      | otherwise -> TyApp ty1' ty2'
      where (ty1',ty2') = (typeHNF ty1,typeHNF ty2)
    TyArr ty1 ty2 -> TyArr (typeHNF ty1) (typeHNF ty2)
    TyBool -> TyBool

typeSubst0 :: Type -> Type -> Type
typeSubst0 s = typeMapVar onvar where
  onvar c k = if c == k then typeShift c s else TyVar k

typeShift :: Int -> Type -> Type
typeShift d = typeMapVar onvar where
  onvar c k = if k >= c then TyVar (k+d) else TyVar k

typeMapVar :: (Int -> Int -> Type) -> Type -> Type
typeMapVar onvar = walk 0 where
  walk c ty = case ty of
    TyVar k -> onvar c k
    TyAll x k ty1 -> TyAll x k (walk (c+1) ty1)
    TyAbs x k ty1 -> TyAbs x k (walk (c+1) ty1)
    TyApp ty1 ty2 -> TyApp (walk c ty1) (walk c ty2)
    TyArr ty1 ty2 -> TyArr (walk c ty1) (walk c ty2)
    TyBool -> TyBool
