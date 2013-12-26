module Main(main) where
import System.Environment(getArgs)
import System.IO(hFlush, stdout)

import Parser
import Syntax
import Top

main :: IO ()
main = do
  files <- getArgs
  txts <- mapM readFile files
  let stmts = fmap concat . sequence $ zipWith parseStmts files txts 
  case stmts >>= execFileStmts [] of
    Left err -> putStrLn err
    Right (Just topCtx') -> repl topCtx'
    Right Nothing -> return ()

execFileStmts :: TopCtx -> [Stmt] -> Either String (Maybe TopCtx)
execFileStmts topCtx [] = Right $ Just topCtx
execFileStmts topCtx (stmt:stmts) = do
  (topCtx',_,continue) <- execStmt topCtx stmt
  if continue
    then execFileStmts topCtx' stmts
    else return Nothing

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
    (mbCtx,io') <- execReplStmts topCtx stmts
    Right $ (mbCtx,io >> io')
  else do
    Right $ (Nothing,io)
  where
    resIO res = case res of
      _ -> putStrLn "..."
