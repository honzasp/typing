module Syntax
( Term(..), Value(..), Type(..), Kind(..)
, TopCtx(..), topCtxEmpty
) where

data Term
  = TmVar Int
  | TmAbs String Type Term
  | TmApp Term Term
  | TmTAbs String Kind Term
  | TmTApp Term Type
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving Show

data Value
  = ValLambda String [Value] Term
  | ValTyLambda String [Value] Term
  | ValBool Bool
  deriving Show

data Type
  = TyVar Int
  | TyAll String Kind Type
  | TyAbs String Kind Type
  | TyApp Type Type
  | TyArr Type Type
  | TyBool
  deriving Show

data Kind
  = KiStar
  | KiArr Kind Kind
  deriving (Show, Eq)

data TopCtx = TopCtx 
  { topCtxValues :: [(String,Value,Type)]
  , topCtxTypes :: [(String,Type,Kind)]
  }

topCtxEmpty :: TopCtx
topCtxEmpty = TopCtx { topCtxValues = [], topCtxTypes = [] }
