module Term(Term(..)) where

data Term
  = TmVar Int
  | TmAbs String Term
  | TmApp Term Term
  deriving Show

