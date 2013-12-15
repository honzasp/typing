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
  | TmUnit
  | TmLet String Term Term
  | TmTuple [Term]
  | TmProj Term Int
  | TmValue Value
  deriving Show

data Value
  = ValAbs String Type Term
  | ValTrue
  | ValFalse
  | ValNat Integer
  | ValUnit
  | ValTuple [Value]
  deriving Show

data Type
  = TyArr Type Type
  | TyBool
  | TyNat
  | TyUnit
  | TyTuple [Type]
  deriving(Show, Eq)
