module Term(Term(..), Type(..), Value(..)) where

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
  | TmValue Value
  deriving Show

data Value
  = ValAbs String Type Term
  | ValTrue
  | ValFalse
  | ValNat Integer
  deriving Show

data Type
  = TyArr Type Type
  | TyBool
  | TyNat
  deriving(Show, Eq)
