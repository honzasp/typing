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
  deriving Show


data Value v
  = ValLambda String (Term v) [Value v]
  | ValBool Bool
  | ValUnit
  deriving Show


data Stmt
  = StmtBind String UnbndTerm
  | StmtTyAbbr String UnbndType
  | StmtEval UnbndTerm
  | StmtCmd Command
  deriving Show

data Command
  = CmdAssert UnbndTerm
  | CmdType UnbndTerm
  | CmdKind UnbndType
  | CmdQuit
  deriving Show


type UnbndTerm = Term String
type UnbndType = Type String
type BndTerm = Term VarBind
type BndType = Type VarBind

type TopCtx = [(String,TopBind)]

data TopBind
  = TopTypeAbbr (Type VarBind) Kind
  | TopTermAbbr (Term VarBind) (Type VarBind)
  deriving Show

data VarBind
  = LocalBind Int
  | TopBind Int
  deriving Show
