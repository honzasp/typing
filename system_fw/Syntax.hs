module Syntax where

data Term v
  = TmVar v
  | TmAbs String (Type v) (Term v)
  | TmApp (Term v) (Term v)
  | TmTAbs String Kind (Term v)
  | TmTApp (Term v) (Type v)
  | TmIf (Term v) (Term v) (Term v)
  | TmAs (Term v) (Type v)
  | TmRcd [(String,Term v)]
  | TmProj (Term v) String
  | TmVariant String (Term v)
  | TmCase (Term v) [(String,String,Term v)]
  | TmTrue 
  | TmFalse
  | TmUnit
  deriving Show

data Type v
  = TyVar v
  | TyAbs String Kind (Type v)
  | TyApp (Type v) (Type v)
  | TyAll String Kind (Type v)
  | TyArr (Type v) (Type v)
  | TyRcd [(String,Type v)]
  | TyVariant [(String,Type v)]
  | TyBool
  | TyUnit
  deriving Show

data Kind = KiStar | KiArr Kind Kind
  deriving (Show, Eq)


data Value v
  = ValAbs String (Term v) [Value v] TopCtx
  | ValTAbs String (Term v) [Value v] TopCtx
  | ValRcd [(String,Value v)]
  | ValVariant String (Value v)
  | ValBool Bool
  | ValUnit
  | ValDummyType
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
  | CmdDump Stmt
  | CmdDumpCtx
  | CmdResolved UnbndTerm
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
