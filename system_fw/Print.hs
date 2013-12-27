{-# LANGUAGE TupleSections #-}
module Print(ppTerm, ppType, ppKind, ppValue) where
import Text.PrettyPrint

import Syntax

ppTerm :: Term String -> Doc
ppTerm = ppTermP (3::Int) where
  ppTermP prec t = if prec' <= prec then doc else parens doc where
    (prec',doc) = case t of
      TmVar x -> (1,) $ text x
      TmAbs x ty1 t2 -> (3,) $
        text "\\" <> text x <>
        text ":" <> ppType ty1 <>
        text "." <> ppTermP 3 t2
      TmApp t1 t2 -> (2,) $
        ppTermP 2 t1 <+> ppTermP 1 t2
      TmTAbs x k1 t2 -> (3,) $
        text "/\\" <> text x <>
        ppOptKind k1 <>
        text "." <> ppTermP 3 t2
      TmTApp t1 ty2 -> (2,) $
        ppTermP 2 t1 <+> brackets (ppType ty2)
      TmIf t1 t2 t3 -> (3,) $
        text "if" <+> ppTermP 2 t1 <+>
        text "then" <+> ppTermP 2 t2 <+>
        text "else" <+> ppTermP 2 t3
      TmTrue -> (1,) $ text "true"
      TmFalse -> (1,) $ text "false"
      TmUnit -> (1,) $ text "unit"

ppType :: Type String -> Doc
ppType = ppTypeP (4::Int) where
  ppTypeP prec ty = if prec' <= prec then doc else parens doc where
    (prec',doc) = case ty of
      TyVar x -> (1,) $ text x
      TyAbs x k1 ty2 -> (4,) $
        text "\\" <> text x <>
        ppOptKind k1 <>
        text "." <> ppTypeP 4 ty2
      TyApp ty1 ty2 -> (2,) $
        ppTypeP 2 ty1 <+> ppTypeP 1 ty2
      TyAll x k1 ty2 -> (4,) $
        text "\\/" <> text x <>
        ppOptKind k1 <>
        text "." <> ppTypeP 4 ty2
      TyArr ty1 ty2 -> (3,) $
        ppTypeP 2 ty1 <> text "->" <> ppTypeP 3 ty2
      TyBool -> (1,) $ text "Bool"
      TyUnit -> (1,) $ text "Unit"

ppOptKind :: Kind -> Doc
ppOptKind KiStar = empty
ppOptKind k = text "::" <> ppKind k

ppKind :: Kind -> Doc
ppKind = ppKindP (2::Int) where
  ppKindP prec k = if prec' <= prec then doc else parens doc where
    (prec',doc) = case k of
      KiStar -> (1,) $ text "*"
      KiArr k1 k2 -> (2,) $
        ppKindP 1 k1 <> text "->" <> ppKindP 2 k2

ppValue :: Value v -> Doc
ppValue v = case v of
  ValAbs {} -> text "#fun"
  ValTAbs {} -> text "#tfun"
  ValBool True -> text "true"
  ValBool False -> text "false"
  ValUnit -> text "unit"
