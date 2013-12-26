module Eval(eval) where
import Syntax

eval :: TopCtx -> Term NameBind -> Value NameBind
eval topCtx t = ValUnit
