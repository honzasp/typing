module Context
  ( Context, Binding(..), InCtx, ctxBind, ctxBindFresh, ctxBound, ctxLookup
  , ctxLookupName , ctxLookupIndex, ctxEmpty
) where
import Data.List (elemIndex)

import Term

type InCtx a = Context -> Either String a

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

