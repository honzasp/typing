module Builtin(builtinCtx) where
import Syntax

type Val = Value NameBind

builtinCtx :: TopCtx
builtinCtx =
  base ++ intFuns ++ floatFuns
  where
  base =
    [ ("true", TopValueBind (wrapB True) tyBool)
    , ("false", TopValueBind (wrapB False) tyBool)
    , ("unit", TopValueBind wrapU tyUnit)

    , ("Int", TopTypeAbbr tyInt KiStar)
    , ("Bool", TopTypeAbbr tyBool KiStar)
    , ("Unit", TopTypeAbbr tyUnit KiStar)
    ]

  intFuns =
    [ ("iadd", TopValueBind (iii (+)) iiiTy)
    , ("isub", TopValueBind (iii (-)) iiiTy)
    , ("imul", TopValueBind (iii (*)) iiiTy)
    , ("idiv", TopValueBind (iii div) iiiTy)
    , ("imod", TopValueBind (iii mod) iiiTy)
    , ("ieq", TopValueBind (iib (==)) iibTy)
    ]
    where iii f = lift2 $ \a b -> wrapI $ unwrapI a `f` unwrapI b
          iiiTy = tyInt --> tyInt --> tyInt
          iib f = lift2 $ \a b -> wrapB $ unwrapI a `f` unwrapI b
          iibTy = tyInt --> tyInt --> tyBool

  floatFuns =
    [ ("fadd", TopValueBind (fff (+)) fffTy)
    , ("fsub", TopValueBind (fff (-)) fffTy)
    , ("fmul", TopValueBind (fff (*)) fffTy)
    , ("fdiv", TopValueBind (fff (/)) fffTy)
    , ("feq", TopValueBind (ffb (==)) ffbTy)
    ]
    where fff f = lift2 $ \a b -> wrapF $ unwrapF a `f` unwrapF b
          fffTy = tyFloat --> tyFloat --> tyFloat
          ffb f = lift2 $ \a b -> wrapB $ unwrapF a `f` unwrapF b
          ffbTy = tyFloat --> tyFloat --> tyBool

  infixr 3 -->
  (-->) = TyArr

  tyInt = TyBase BTyInt
  tyFloat = TyBase BTyFloat
  tyBool = TyBase BTyBool
  tyUnit = TyBase BTyUnit

  wrapI = ValBase . BValInt
  unwrapI (ValBase (BValInt i)) = i
  unwrapI _ = error "Builtin function expected Int"

  wrapF = ValBase . BValFloat
  unwrapF (ValBase (BValFloat f)) = f
  unwrapF _ = error "Builtin function expected Float"

  wrapB = ValBase . BValBool
  --unwrapB (ValBase (BValBool b)) = b
  --unwrapB _ = error "Builtin function expected Bool"

  wrapU = ValBase $ BValUnit

  lift2 :: (Val -> Val -> Val) -> Val
  lift2 f =
    ValFun . BuiltinFun $ \a ->
      ValFun . BuiltinFun $ \b ->
        f a b
