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
  = CmdResShow Value Type
  | CmdResBound String Term Type
  | CmdResType Type
  | CmdResDebug String
  | CmdResEmpty
  | CmdResQuit
  deriving Show

type ModCtx = (NameCtx,TypeCtx)

evalCmd :: ModCtx -> Command -> Either String (ModCtx,CmdResult)
evalCmd modCtx@(nameCtx,typeCtx) cmd = case cmd of
  CmdBindTerm x termInCtx -> do
    term <- termInCtx nameCtx
    ty <- typeOf typeCtx term
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
      "a" -> assertion term
      "dp" -> debugRaw term
      "de" -> debugEval term
      _   -> Left $ "Undefined 1-special: " ++ show spec
  CmdEmpty -> Right (modCtx,CmdResEmpty)
  where
    debugRaw = Right . (modCtx,) . CmdResDebug . show
    debugEval = Right . (modCtx,) . CmdResDebug . show . eval nameCtx

    assertion t = do
      ty <- typeOf typeCtx t
      case ty of 
        TyBool -> Right ()
        _      -> Left $ "Assertion type must be Bool"
      case eval nameCtx t of
        ValTrue  -> Right (modCtx,CmdResEmpty)
        ValFalse -> Left $ "Assertion failed: " ++ render (ppTerm nameCtx t)
        other    -> error $ "Term of type Bool didn't evaluate to true or false"
