module Command(Command(..), CmdResult(..), evalCmd) where
import Text.PrettyPrint(render)

import Context
import Eval
import Print
import Term

data Command
  = CmdBindName String
  | CmdBindTerm String (InCtx Term)
  | CmdEvalTerm (InCtx Term)
  | CmdSpecial0 String
  | CmdSpecial1 String (InCtx Term)
  | CmdEmpty

data CmdResult
  = CmdResShow String
  | CmdResPrint String
  | CmdResBound String (Maybe String)
  | CmdResEmpty
  | CmdResQuit
  deriving Show

evalCmd :: Context -> Command -> Either String (Context,CmdResult)
evalCmd ctx cmd = case cmd of
  CmdBindName x -> Right (ctxBind x NameBind ctx,CmdResBound x Nothing)
  CmdBindTerm x termInCtx -> do
    term <- termInCtx ctx
    let termTxt = render $ ppTerm ctx term
    Right (ctxBind x (TermBind term) ctx,CmdResBound x (Just termTxt))
  CmdEvalTerm termInCtx -> do
    term <- termInCtx ctx
    let valueTxt = render $ ppTerm ctx (eval ctx term)
    Right (ctx,CmdResShow valueTxt)
  CmdSpecial0 spec -> case spec of
    "q" -> Right (ctx,CmdResQuit)
    _other -> Left $ "Undefined 0-special: " ++ show spec
  CmdSpecial1 spec termInCtx -> do
    term <- termInCtx ctx
    case spec of
      "p" -> Right(ctx,CmdResPrint $ render $ ppTerm ctx (eval ctx term))
      _other -> Left $ "Undefined 1-special: " ++ show spec
  CmdEmpty -> Right (ctx,CmdResEmpty)
