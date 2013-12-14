{-# LANGUAGE TupleSections #-}
module Main(main) where
import Control.Applicative((<$>))
import Control.Monad
import System.Environment
import System.IO
import Text.PrettyPrint(render)

import Parser
import Syntax
import Eval

main :: IO ()
main = do
  files <- getArgs
  mods <- mapM openMod files
  case foldM importMod ctxEmpty mods of
    Left (ctx',err) -> do
      putStrLn $ "Error opening mods: " ++ err
      replLoop ctx'
    Right ctx' -> replLoop ctx'

type Mod = (FilePath,String)

openMod :: FilePath -> IO Mod
openMod path = (path,) <$> readFile path

importMod :: Context -> Mod -> Either (Context,String) Context
importMod ctx (path,txt) =
  case tagError "Parse err: " $ parseMod txt of
    Right cmds -> case foldM (\ctx cmd -> fst <$> evalCmd ctx cmd) ctx cmds of
      Right ctx -> Right ctx
      Left err -> Left (ctx,err)
    Left err -> Left (ctx,err)

replLoop :: Context -> IO ()
replLoop ctx = do
  putStr "> " >> hFlush stdout
  cmd <- tagError "Parse err: " . parseCommand <$> getLine
  case cmd >>= evalCmd ctx of
    Left err -> do
      putStrLn err
      replLoop ctx
    Right (ctx',res) -> case res of
      CmdResShow txt -> 
        putStrLn txt >> replLoop ctx'
      CmdResBound x Nothing ->
        putStrLn x >> replLoop ctx'
      CmdResBound x (Just t) -> 
        putStrLn (x ++ " = " ++ t) >> replLoop ctx'
      CmdResEmpty ->
        replLoop ctx'
      CmdResQuit ->
        return ()

evalCmd :: Context -> Command -> Either String (Context,CmdResult)
evalCmd ctx cmd = case cmd of
  CmdBindName x -> Right (ctxBind x NameBind ctx,CmdResBound x Nothing)
  CmdBindTerm x termInCtx -> do
    term <- tagError "Name err: " $ termInCtx ctx
    let termTxt = render $ ppTerm ctx term
    Right (ctxBind x (TermBind term) ctx,CmdResBound x (Just termTxt))
  CmdEvalTerm termInCtx -> do
    term <- tagError "Name err: " $ termInCtx ctx
    let valueTxt = render $ ppTerm ctx (eval ctx term)
    Right (ctx,CmdResShow valueTxt)
  CmdSpecial spec -> case spec of
    "q" -> Right (ctx,CmdResQuit)
    other -> Left $ "Undefined special: " ++ spec
  CmdEmpty -> Right (ctx,CmdResEmpty)
      
tagError :: String -> Either String a -> Either String a
tagError tag (Left e) = Left (tag ++ e)
tagError _   (Right x) = Right x
