{-# LANGUAGE TupleSections #-}
module Print(ppTerm, ppValue, ppType, ppValueType, ppNameTermType) where
import Text.PrettyPrint

import Context
import Term

ppTerm :: NameCtx -> Term -> Doc
ppTerm = ppTerm' 5

ppTerm' :: Int -> NameCtx -> Term -> Doc
ppTerm' prec ctx t = if prec' <= prec then doc else parens doc where 
  (prec',doc) = case t of
    TmVar idx        -> (0,) . text $ ctxLookupName idx ctx
    TmAbs hint ty t1 -> (4,) $ ppAbs ctx hint ty t1
    TmApp t1 t2      -> (2,) $ ppTerm' 2 ctx t1 <+> ppTerm' 1 ctx t2
    TmTrue           -> (0,) $ text "true"
    TmFalse          -> (0,) $ text "false"
    TmIf t1 t2 t3    -> (4,) $
      text "if" <+> ppTerm' 4 ctx t1 <+>
      text "then" <+> ppTerm' 4 ctx t2 <+>
      text "else" <+> ppTerm' 4 ctx t3
    TmNat n          -> (0,) $ text (show n)
    TmSucc t1        -> (2,) $ text "succ" <+> ppTerm' 1 ctx t1
    TmPred t1        -> (2,) $ text "pred" <+> ppTerm' 1 ctx t1
    TmIszero t1      -> (2,) $ text "iszero" <+> ppTerm' 1 ctx t1
    TmUnit           -> (0,) $ text "unit"
    TmLet hint t1 t2 -> (4,) $ ppLet ctx hint t1 t2
    TmTuple ts       -> (0,) $
      braces . hcat . punctuate (text ",") . map (ppTerm' 5 ctx) $ ts
    TmProj t j       -> (1,) $ ppTerm' 0 ctx t <> text "." <> text (show j)
    TmFix t1         -> (4,) $ text "fix" <+> ppTerm' 4 ctx t1
    TmValue val      -> (prec,) $ ppValue' prec ctx val

ppValue :: NameCtx -> Value -> Doc
ppValue = ppValue' 5

ppValue' :: Int -> NameCtx -> Value -> Doc
ppValue' prec ctx val = if prec' <= prec then doc else parens doc where
  (prec',doc) = case val of
    ValAbs hint ty t1 -> (4,) $ ppAbs ctx hint ty t1
    ValTrue           -> (0,) $ text "true"
    ValFalse          -> (0,) $ text "false"
    ValNat n          -> (0,) $ text (show n)
    ValUnit           -> (0,) $ text "unit"
    ValTuple vs       -> (0,) .
      braces . hcat . punctuate (text ",") . map (ppValue ctx) $ vs

ppAbs :: NameCtx -> String -> Type -> Term -> Doc
ppAbs ctx hint ty t1 =
  let (name,ctx') = ctxBindFreshName hint NBndNameBind ctx
  in  text "\\" <> text name <>
      text ":" <> ppType ty <>
      text "." <> ppTerm' 4 ctx' t1

ppLet :: NameCtx -> String -> Term -> Term -> Doc
ppLet ctx hint t1 t2 =
  let (name,ctx') = ctxBindFreshName hint NBndNameBind ctx
  in  text "let" <+> text name <>
      text "=" <> ppTerm' 4 ctx t1 <+>
      text "in" <+> ppTerm' 4 ctx' t2

ppType :: Type -> Doc
ppType ty = case ty of
  TyArr ty1 ty2 -> ppType ty1 <+> text "->" <+> ppType ty2
  TyBool -> text "Bool"
  TyNat -> text "Nat"
  TyUnit -> text "Unit"
  TyTuple tys -> braces . hcat . punctuate (text ",") . map ppType $ tys

ppValueType :: NameCtx -> Value -> Type -> Doc
ppValueType ctx v ty = ppValue ctx v <+> text ":" <+> ppType ty

ppNameTermType :: NameCtx -> String -> Term -> Type -> Doc
ppNameTermType ctx x t ty =
  text x <+> text "=" <+> ppTerm ctx t <+> text ":" <+> ppType ty
