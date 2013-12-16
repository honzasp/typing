{-# LANGUAGE TupleSections #-}
module Command
  ( Command(..), CmdSpecial(..), CmdResult(..), ModCtx, evalCmd
) where
import Control.Applicative((<$>))
import Text.PrettyPrint(render)

import Context
import Eval
import Print
import Term
import Typing

data Command
  = CmdBindTerm String (InNameCtx Term)
  | CmdEvalTerm (InNameCtx Term)
  | CmdSpecial CmdSpecial

data CmdSpecial
  = CmdSpecQuit
  | CmdSpecType (InNameCtx Term)
  | CmdSpecAssert (InNameCtx Term)
  | CmdSpecDbgParsed (InNameCtx Term)

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
  CmdSpecial spec -> case spec of
    CmdSpecQuit -> Right (modCtx,CmdResQuit)
    CmdSpecType tInCtx ->
      tInCtx nameCtx >>=
      typeOf typeCtx >>=
      Right . (modCtx,) . CmdResType
    CmdSpecAssert tInCtx -> assertion tInCtx
    CmdSpecDbgParsed tInCtx ->
      (modCtx,) . CmdResDebug . show <$> tInCtx nameCtx
  where
    assertion tInCtx = do
      t <- tInCtx nameCtx
      ty <- typeOf typeCtx t
      case ty of 
        TyBool -> Right ()
        _      -> Left $ "Assertion type must be Bool"
      case eval nameCtx t of
        ValTrue  -> Right (modCtx,CmdResEmpty)
        ValFalse -> Left $ "Assertion failed: " ++ render (ppTerm nameCtx t)
        other    -> error $ "Term of type Bool didn't evaluate to true or false"
