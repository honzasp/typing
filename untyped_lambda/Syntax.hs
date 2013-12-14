module Syntax
  ( Term(..), Command(..), CmdResult(..), InCtx, ppTerm
  , Context, Binding(..), ctxBind, ctxBindFresh, ctxBound, ctxLookup
  , ctxLookupName , ctxLookupIndex, ctxEmpty
) where
import Data.List (elemIndex)
import Control.Applicative ((<$>), (<*>))
import Text.PrettyPrint

type InCtx a = Context -> Either String a

data Term
  = TmVar Int
  | TmAbs String Term
  | TmApp Term Term
  deriving Show

data Command
  = CmdBindName String
  | CmdBindTerm String (InCtx Term)
  | CmdEvalTerm (InCtx Term)
  | CmdSpecial String
  | CmdEmpty

data CmdResult
  = CmdResShow String
  | CmdResBound String (Maybe String)
  | CmdResEmpty
  | CmdResQuit
  deriving Show

ppTerm :: Context -> Term -> Doc
ppTerm ctx t = case t of
  TmVar idx ->
    text $ ctxLookupName idx ctx
  TmAbs hint t1 -> do
    let (name,ctx') = ctxBindFresh hint NameBind ctx
    (text "\\" <> text name <> text "." <>) . parens $ ppTerm ctx' t1
  TmApp t1 t2 ->
    ppTerm ctx t1 <+> ppTerm ctx t2

data Context = Context 
  { ctxBindings :: [(String,Binding)] }
  deriving Show

data Binding
  = NameBind
  | TermBind Term
  deriving Show

ctxBind :: String -> Binding -> Context -> Context
ctxBind x bnd ctx = ctx { ctxBindings = (x,bnd):ctxBindings ctx }

ctxBindFresh :: String -> Binding -> Context -> (String,Context)
ctxBindFresh x bnd ctx =
  if ctxBound x ctx 
    then ctxBindFresh (x++"'") bnd ctx
    else (x,ctxBind x bnd ctx)

ctxBound :: String -> Context -> Bool
ctxBound x = (x `elem`) . map fst . ctxBindings

ctxLookup :: Int -> Context -> Binding
ctxLookup k = snd . (!! k) . ctxBindings

ctxLookupName :: Int -> Context -> String
ctxLookupName k = fst . (!! k) . ctxBindings

ctxLookupIndex :: String -> Context -> Either String Int
ctxLookupIndex x ctx = case elemIndex x (map fst $ ctxBindings ctx) of
  Just k -> Right k
  Nothing -> Left $ "Variable `" ++ x ++ "` not in context"

ctxEmpty :: Context
ctxEmpty = Context { ctxBindings = [] }

{-
ctxLookupIdx :: String -> Context -> Either String Int
ctxLookupIdx x ctx = case elemIndex x (ctxBindings ctx) of
  Just idx -> Right idx
  Nothing -> Left $ "Variable " ++ x ++ " not in context"

ctxBound :: String -> Context -> Bool
ctxBound x ctx = x `elem` ctxBindings ctx

ctxLookupName :: Int -> Context -> Either String String
ctxLookupName idx ctx
  | length bndgs > idx = Right $ bndgs !! idx
  | otherwise          = Left $ "Index " ++ show idx ++ " too large"
  where bndgs = ctxBindings ctx

ctxBind :: String -> Context -> Context
ctxBind x ctx = ctx { ctxBindings = x:ctxBindings ctx }

ctxBindFresh :: String -> Context -> (String,Context)
ctxBindFresh hint ctx
  | ctxBound hint ctx = ctxBindFresh (hint ++ "'") ctx
  | otherwise         = (hint,ctxBind hint ctx)

ctxEmpty :: Context
ctxEmpty = Context { ctxBindings = [] }
-}
