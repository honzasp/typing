module Term(Term(..), Type(..)) where

data Term
  = TmVar Int
  | TmAbs String Type Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIszero Term
  deriving Show

data Type
  = TyArr Type Type
  | TyBool
  | TyNat
  deriving(Show, Eq)
