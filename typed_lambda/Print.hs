module Print(ppTerm, ppType, ppTermType, ppNameTermType) where
import Text.PrettyPrint

import Context
import Term

ppTerm :: NameCtx -> Term -> Doc
ppTerm ctx t = case t of
  TmVar idx ->
    text $ ctxLookupName idx ctx
  TmAbs hint ty t1 -> do
    let (name,ctx') = ctxBindFreshName hint NBndNameBind ctx
    (text "\\" <> text name <> text ":" <> ppType ty <> text "." <>) 
      . parens $ ppTerm ctx' t1
  TmApp t1 t2 ->
    ppTerm ctx t1 <+> ppTerm ctx t2
  TmTrue -> text "true"
  TmFalse -> text "false"
  TmIf t1 t2 t3 ->
    text "if" <+> ppTerm ctx t1 <+>
    text "then" <+> ppTerm ctx t2 <+>
    text "else" <+> ppTerm ctx t3
  TmZero -> text "0"
  TmSucc t1 -> text "succ" <+> ppTerm ctx t1
  TmPred t1 -> text "pred" <+> ppTerm ctx t1
  TmIszero t1 -> text "iszero" <+> ppTerm ctx t1

ppType :: Type -> Doc
ppType ty = case ty of
  TyArr ty1 ty2 -> ppType ty1 <+> text "->" <+> ppType ty2
  TyBool -> text "Bool"
  TyNat -> text "Nat"

ppTermType :: NameCtx -> Term -> Type -> Doc
ppTermType ctx t ty = ppTerm ctx t <+> text ":" <+> ppType ty

ppNameTermType :: NameCtx -> String -> Term -> Type -> Doc
ppNameTermType ctx x t ty =
  text x <+> text "=" <+> ppTerm ctx t <+> text ":" <+> ppType ty
