{-# LANGUAGE TupleSections #-}
module Command(Command(..), CmdResult(..), ModCtx, evalCmd) where
import Text.PrettyPrint(render)

import Context
import Eval
import Print
import Term
import Typing

data Command
  = CmdBindTerm String (InNameCtx Term)
  | CmdEvalTerm (InNameCtx Term)
  | CmdSpecial0 String
  | CmdSpecial1 String (InNameCtx Term)
  | CmdEmpty

data CmdResult
  = CmdResShow Term Type
  | CmdResBound String Term Type
  | CmdResType Type
  | CmdResEmpty
  | CmdResQuit
  deriving Show

type ModCtx = (NameCtx,TypeCtx)

evalCmd :: ModCtx -> Command -> Either String (ModCtx,CmdResult)
evalCmd modCtx@(nameCtx,typeCtx) cmd = case cmd of
  CmdBindTerm x termInCtx -> do
    term <- termInCtx nameCtx
    ty <- typeOf typeCtx term
    let value = eval nameCtx term
    let nameCtx' = ctxBind (x,NBndTermBind term) nameCtx
    let typeCtx' = ctxBind (TBndVarBind ty) typeCtx
    Right ((nameCtx',typeCtx'),CmdResBound x term ty)
  CmdEvalTerm termInCtx -> do
    term <- termInCtx nameCtx
    ty <- typeOf typeCtx term
    let value = eval nameCtx term
    Right (modCtx,CmdResShow value ty)
  CmdSpecial0 spec -> case spec of
    "q" -> Right (modCtx,CmdResQuit)
    _   -> Left $ "Undefined 0-special: " ++ show spec
  CmdSpecial1 spec termInCtx -> do
    term <- termInCtx nameCtx
    case spec of
      "t" -> typeOf typeCtx term >>= Right . (modCtx,) . CmdResType
      _   -> Left $ "Undefined 1-special: " ++ show spec
  CmdEmpty -> Right (modCtx,CmdResEmpty)
