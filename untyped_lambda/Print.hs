module Print(ppTerm) where
import Text.PrettyPrint

import Context
import Term

ppTerm :: Context -> Term -> Doc
ppTerm ctx t = case t of
  TmVar idx ->
    text $ ctxLookupName idx ctx
  TmAbs hint t1 -> do
    let (name,ctx') = ctxBindFresh hint NameBind ctx
    (text "\\" <> text name <> text "." <>) . parens $ ppTerm ctx' t1
  TmApp t1 t2 ->
    ppTerm ctx t1 <+> ppTerm ctx t2
