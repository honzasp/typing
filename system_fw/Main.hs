module Main(main) where
import Control.Applicative
import qualified Data.List as L
import System.Environment(getArgs)
import System.IO(hFlush, stdout)
import Text.PrettyPrint

import Naming
import Parser
import Print
import Syntax
import Top

main :: IO ()
main = do
  files <- getArgs
  txts <- mapM readFile files
  let stmts = fmap concat . sequence $ zipWith parseStmts files txts 
  let topCtx = []
  case execFileStmts topCtx <$> stmts of
    Left err -> putStrLn err >> repl topCtx
    Right (topCtx',r) -> case r of
      Left err -> putStrLn err >> repl topCtx'
      Right True -> repl topCtx'
      Right False -> return ()

execFileStmts :: TopCtx -> [Stmt] -> (TopCtx,Either String Bool)
execFileStmts topCtx [] = (topCtx,Right True)
execFileStmts topCtx (stmt:stmts) = do
  case execStmt topCtx stmt of
    Right (topCtx',_,continue) -> if continue
      then execFileStmts topCtx' stmts
      else (topCtx',Right False)
    Left err -> (topCtx,Left err)

repl :: TopCtx -> IO ()
repl topCtx = do
  putStr "> " >> hFlush stdout
  line <- getLine
  case parseStmts "<repl>" line >>= execReplStmts topCtx of
    Left err ->
      putStrLn err >> repl topCtx
    Right (Just topCtx',io) ->
      io >> repl topCtx'
    Right (Nothing,io) ->
      io

execReplStmts :: TopCtx -> [Stmt] -> Either String (Maybe TopCtx,IO ())
execReplStmts topCtx [] = Right $ (Just topCtx,return ())
execReplStmts topCtx (stmt:stmts) = do
  (topCtx',res,continue) <- execStmt topCtx stmt
  let io = resIO res
  if continue then do
    (mbCtx,io') <- execReplStmts topCtx' stmts
    Right $ (mbCtx,io >> io')
  else do
    Right $ (Nothing,io)
  where
    resIO res = case res of
      ResTermBound x t ty -> putStrLn . render $
        text x <+> text "=" <+> pTerm t <+> text ":" <+> pType ty
      ResValueBound x v ty -> putStrLn . render $
        text x <+> text "<-" <+> pValue v <+> text ":" <+> pType ty
      ResTypeBound x ty k -> putStrLn . render $
        text x <+> text ":=" <+> pType ty <+> text "::" <+> ppKind k
      ResEval v ty -> putStrLn . render $
        pValue v <+> text ":" <+> pType ty
      ResShowType ty -> putStrLn . render $ pType ty
      ResShowKind k -> putStrLn . render $ ppKind k
      ResShowCtx ctx -> putStrLn . render . vcat $ pCtx ctx
      ResDumpStmt stmt -> putStrLn . show $ stmt
      ResDumpCtx ctx -> putStrLn . show $ ctx
      ResDumpTerm t -> putStrLn . show $ t
      ResOk -> return ()

    pTerm = ppTerm . renameTerm topCtx
    pType = ppType . renameType topCtx
    pValue = ppValue

    pTopBind (x,bnd) ctx = case bnd of
      TopTermAbbr t ty ->
        text x <+> text "=" <+> ppTerm (renameTerm ctx t) <+>
        text ":" <+> ppType (renameType ctx ty)
      TopValueBind v ty ->
        text x <+> text "<-" <+> ppValue (renameValue ctx v) <+>
        text ":" <+> ppType (renameType ctx ty)
      TopTypeAbbr ty k ->
        text x <+> text ":=" <+> ppType (renameType ctx ty) <+>
        text "::" <+> ppKind k

    pCtx ctx = reverse [pTopBind (x,bnd) ctx' | ((x,bnd):ctx') <- L.tails ctx]
