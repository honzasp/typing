module Syntax where

data Term v
  = TmVar v
  | TmAbs String (Type v) (Term v)
  | TmApp (Term v) (Term v)
  | TmTAbs String Kind (Term v)
  | TmTApp (Term v) (Type v)
  | TmIf (Term v) (Term v) (Term v)
  | TmTrue | TmFalse
  | TmUnit
  deriving Show

data Type v
  = TyVar v
  | TyAbs String Kind (Type v)
  | TyApp (Type v) (Type v)
  | TyAll String Kind (Type v)
  | TyArr (Type v) (Type v)
  | TyBool
  | TyUnit
  deriving Show

data Kind = KiStar | KiArr Kind Kind
  deriving (Show, Eq)


data Value v
  = ValLambda String (Term v) [Value v]
  | ValBool Bool
  | ValUnit
  deriving Show


data Stmt
  = StmtTermAbbr String UnbndTerm
  | StmtTypeAbbr String UnbndType
  | StmtEval UnbndTerm
  | StmtCmd Command
  deriving Show

data Command
  = CmdAssert UnbndTerm
  | CmdType UnbndTerm
  | CmdKind UnbndType
  | CmdCtx
  | CmdQuit
  deriving Show


type UnbndTerm = Term String
type UnbndType = Type String
type BndTerm = Term NameBind
type BndType = Type NameBind

type TopCtx = [(String,TopBind)]

data TopBind
  = TopTermAbbr (Term NameBind) (Type NameBind)
  | TopTypeAbbr (Type NameBind) Kind
  deriving Show

data NameBind
  = LocalBind Int
  | TopBind Int
  deriving (Show, Eq)
