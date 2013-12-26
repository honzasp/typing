module Main(main) where
import Naming
import Parser
import Syntax
import Typing

main :: IO ()
main = do
  let p = parseStmts "<input>" "\\x:(\\a::*.a) Bool.b"
  let Right ([StmtEval unbndTerm]) = p
  let topCtx = [("b",TopTermAbbr (TmTrue) (TyBool))]
  let Right t = resolveTerm topCtx unbndTerm
  print $ typeOf topCtx t
