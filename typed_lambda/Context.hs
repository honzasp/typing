module Context
  ( Context, NameBinding(..), TypeBinding(..)
  , InNameCtx, NameCtx, TypeCtx
  , ctxBind, ctxLookup, ctxEmpty, ctxMap, ctxBindings
  , ctxBindFreshName, ctxBoundName, ctxLookupName, ctxLookupIndex
) where
import Data.List (elemIndex)

import Term

type InNameCtx a = NameCtx -> Either String a

data Context b = Context 
  { ctxBindings :: [b] }
  deriving Show

type NameCtx = Context (String,NameBinding)
type TypeCtx = Context TypeBinding

data NameBinding
  = NBndNameBind
  | NBndTermBind Term
  deriving Show

data TypeBinding
  = TBndVarBind Type
  deriving Show

ctxBind :: b -> Context b -> Context b
ctxBind bnd ctx = ctx { ctxBindings = bnd:ctxBindings ctx }

ctxLookup :: Int -> Context b -> b
ctxLookup k = (!! k) . ctxBindings

ctxEmpty :: Context b
ctxEmpty = Context { ctxBindings = [] }

ctxMap :: (a -> b) -> Context a -> Context b
ctxMap f ctx = Context { ctxBindings = map f (ctxBindings ctx) }

ctxBindFreshName :: String -> b -> Context (String,b) -> (String,Context (String,b))
ctxBindFreshName x bnd ctx =
  if ctxBoundName x ctx 
    then ctxBindFreshName (x++"'") bnd ctx
    else (x,ctxBind (x,bnd) ctx)

ctxBoundName :: String -> Context (String,b) -> Bool
ctxBoundName x = (x `elem`) . map fst . ctxBindings

ctxLookupName :: Int -> Context (String,b) -> String
ctxLookupName k = fst . (!! k) . ctxBindings

ctxLookupIndex :: String -> Context (String,b) -> Either String Int
ctxLookupIndex x ctx = case elemIndex x (map fst $ ctxBindings ctx) of
  Just k -> Right k
  Nothing -> Left $ "Variable `" ++ x ++ "` not in context"
