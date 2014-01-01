module Top where
import Eval
import Naming
import Syntax
import Typing

data StmtRes
  = ResTermBound String (Term NameBind) (Type NameBind)
  | ResValueBound String (Value NameBind) (Type NameBind)
  | ResTypeBound String (Type NameBind) Kind
  | ResEval (Value NameBind) (Type NameBind)
  | ResShowType (Type NameBind)
  | ResShowKind Kind
  | ResShowCtx TopCtx
  | ResDumpStmt Stmt
  | ResDumpCtx TopCtx
  | ResDumpTerm (Term NameBind)
  | ResOk
  deriving Show

execStmt :: TopCtx -> Stmt -> Either String (TopCtx,StmtRes,Bool)
execStmt topCtx stmt = case stmt of
  StmtTermAbbr abbr uTerm -> do
    (t,ty) <- termType uTerm
    Right ((abbr,TopTermAbbr t ty):topCtx,ResTermBound abbr t ty,True)
  StmtValueBind name uTerm -> do
    (t,ty) <- termType uTerm
    let v = evaluate topCtx t
    Right ((name,TopValueBind v ty):topCtx,ResValueBound name v ty,True)
  StmtTypeAbbr abbr uType -> do
    (ty,k) <- typeKind uType
    Right ((abbr,TopTypeAbbr ty k):topCtx,ResTypeBound abbr ty k,True)
  StmtEval uTerm -> do
    (t,ty) <- termType uTerm
    Right (topCtx,ResEval (evaluate topCtx t) ty,True)
  StmtCmd cmd -> case cmd of
    CmdAssert uTerm -> do
      (t,ty) <- termType uTerm
      case ty of
        TyBool -> Right ()
        _      -> Left $ "Assertion type must be Bool"
      case evaluate topCtx t of
        ValBool True -> Right (topCtx,ResOk,True)
        _            -> Left $ "Assertion failed"
    CmdType uTerm -> do
      (_,ty) <- termType uTerm
      Right (topCtx,ResShowType ty,True)
    CmdKind uType -> do
      (_,k) <- typeKind uType
      Right (topCtx,ResShowKind k,True)
    CmdCtx ->
      Right (topCtx,ResShowCtx topCtx,True)
    CmdDump stmt ->
      Right (topCtx,ResDumpStmt stmt,True)
    CmdDumpCtx ->
      Right (topCtx,ResDumpCtx topCtx,True)
    CmdResolved uTerm -> do
      t <- resolveTerm topCtx uTerm
      Right (topCtx,ResDumpTerm t,True)
    CmdQuit ->
      Right (topCtx,ResOk,False)

  where
  termType :: Term String -> Either String (Term NameBind,Type NameBind)
  termType uTerm = do
    t <- resolveTerm topCtx uTerm
    ty <- typeOf topCtx t
    Right $ (t,ty)

  typeKind :: Type String -> Either String (Type NameBind,Kind)
  typeKind uType = do
    ty <- resolveType topCtx uType
    k <- kindOf topCtx ty
    Right $ (ty,k)
