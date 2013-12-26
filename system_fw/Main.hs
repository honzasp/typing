module Main(main) where
import Naming
import Parser
import Syntax

main :: IO ()
main = do
  let p = parseStmts "<input>" "\\x:(\\a::*.a) Bool.b"
  print p
  let Right ([StmtEval unbndTerm]) = p
  let topCtx = [("b",TopTermAbbr (TmTrue) (TyBool))]
  print $ resolveTerm topCtx unbndTerm
