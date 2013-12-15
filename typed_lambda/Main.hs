{-# LANGUAGE TupleSections #-}
module Main(main) where
import Control.Applicative((<$>))
import Control.Monad.Instances()
import System.Environment(getArgs)
import System.IO(hFlush, stdout)
import Prelude hiding(mod)
import Text.PrettyPrint(render)

import Command
import Context
import Parser
import Print

main :: IO ()
main = do
  files <- getArgs
  r <- mapM openMod files >>= evalMods (ctxEmpty,ctxEmpty)
  case r of
    Left (ctx,err) -> putStrLn err >> replLoop ctx
    Right ctx -> replLoop ctx

type Mod = (FilePath,String)

openMod :: FilePath -> IO Mod
openMod path = (path,) <$> readFile path

evalMods :: ModCtx -> [Mod] -> IO (Either (ModCtx,String) ModCtx)
evalMods ctx [] = return . Right $ ctx
evalMods ctx (mod:mods) = do
  r <- evalMod ctx mod
  case r of
    Right ctx' -> evalMods ctx' mods
    Left err -> return $ Left (ctx,err)

evalMod :: ModCtx -> Mod -> IO (Either String ModCtx)
evalMod ctx (path,txt) = do
  case parseMod txt of
    Left err -> return $ Left err
    Right cmds -> modSteps ctx cmds
  where
    modSteps :: ModCtx -> [Command] -> IO (Either String ModCtx)
    modSteps ctx [] = return $ Right ctx
    modSteps ctx (cmd:cmds) = do
      r <- modStep ctx cmd
      case r of
        Left err -> return $ Left err
        Right ctx' -> modSteps ctx' cmds

    modStep :: ModCtx -> Command -> IO (Either String ModCtx)
    modStep ctx cmd = case evalCmd ctx cmd of
      Left err -> return $ Left err
      Right (ctx',res) -> do
        case res of
          CmdResQuit -> return $ Left "Module tried to quit"
          _ -> return $ Right ctx'

replLoop :: ModCtx -> IO ()
replLoop ctx = do
  putStr "> " >> hFlush stdout
  cmdRes <- parseCommand <$> getLine
  case cmdRes of
    Left parseErr -> putStrLn parseErr >> replLoop ctx
    Right cmd -> do
      maybeCtx <- replStep ctx cmd
      case maybeCtx of
        Just ctx' -> replLoop ctx'
        Nothing -> return ()

  where
    replStep :: ModCtx -> Command -> IO (Maybe ModCtx)
    replStep ctx@(nameCtx,_) cmd = case evalCmd ctx cmd of
      Left err -> putStrLn err >> return (Just ctx)
      Right (ctx',res) -> do
        continue <- case res of
          CmdResShow term ty -> 
            putStrLn (render $ ppTermType nameCtx term ty) >> return True
          CmdResBound x term ty -> 
            putStrLn (render $ ppNameTermType nameCtx x term ty) >> return True
          CmdResType ty ->
            putStrLn (render $ ppType ty) >> return True
          CmdResEmpty ->
            return True
          CmdResQuit ->
            return False
        if continue then return (Just ctx') else return Nothing
