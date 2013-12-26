module Main(main) where
import Parser
import Syntax
import Top

main :: IO ()
main = do
  let p = parseStmts "<input>" "\\x:(\\a::*.a) Bool.b"
  let Right ([stmt]) = p
  let topCtx = [("b",TopTermAbbr (TmTrue) (TyBool))]
  print $ execStmt topCtx stmt
