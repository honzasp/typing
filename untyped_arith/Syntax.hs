module Syntax(Term(..), Value(..), ppTerm, ppValue) where

import Text.PrettyPrint(Doc, text, hsep, (<+>))

data Term =
    TmTrue 
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Show, Eq)

ppTerm :: Term -> Doc
ppTerm TmTrue = text "true"
ppTerm TmFalse = text "false"
ppTerm (TmIf t1 t2 t3) =
  hsep [text "if", ppTerm t1, text "then", ppTerm t2, text "else", ppTerm t3]
ppTerm TmZero = text "0"
ppTerm (TmSucc t) = text "succ" <+> ppTerm t
ppTerm (TmPred t) = text "pred" <+> ppTerm t
ppTerm (TmIsZero t) = text "iszero" <+> ppTerm t

data Value =
    ValTrue
  | ValFalse
  | ValNv Integer
  deriving (Show, Eq)

ppValue :: Value -> Doc
ppValue ValTrue = text "true"
ppValue ValFalse = text "false"
ppValue (ValNv n) = nv n
  where nv 0 = text "0"
        nv n = text "succ" <+> nv (n-1)
