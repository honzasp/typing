{-# LANGUAGE TupleSections #-}
module Main(main) where
import Control.Applicative((<$>))
import Control.Monad
import Control.Monad.Instances()
import System.Environment
import System.IO

import Command
import Context
import Parser

main :: IO ()
main = do
  files <- getArgs
  r <- mapM openMod files >>= evalMods ctxEmpty
  case r of
    Left (ctx,err) -> putStrLn err >> replLoop ctx
    Right ctx -> replLoop ctx

type Mod = (FilePath,String)

openMod :: FilePath -> IO Mod
openMod path = (path,) <$> readFile path

evalMods :: Context -> [Mod] -> IO (Either (Context,String) Context)
evalMods ctx [] = return . Right $ ctx
evalMods ctx (mod:mods) = do
  r <- evalMod ctx mod
  case r of
    Right ctx' -> evalMods ctx' mods
    Left err -> return $ Left (ctx,err)

evalMod :: Context -> Mod -> IO (Either String Context)
evalMod ctx (path,txt) = do
  case parseMod txt of
    Left err -> return $ Left err
    Right cmds -> modSteps ctx cmds
  where
    modSteps :: Context -> [Command] -> IO (Either String Context)
    modSteps ctx [] = return $ Right ctx
    modSteps ctx (cmd:cmds) = do
      r <- modStep ctx cmd
      case r of
        Left err -> return $ Left err
        Right ctx' -> modSteps ctx' cmds

    modStep :: Context -> Command -> IO (Either String Context)
    modStep ctx cmd = case evalCmd ctx cmd of
      Left err -> return $ Left err
      Right (ctx',res) -> do
        case res of
          CmdResPrint txt -> putStrLn txt
          _ -> return ()
        return $ Right ctx'

replLoop :: Context -> IO ()
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
    replStep :: Context -> Command -> IO (Maybe Context)
    replStep ctx cmd = case evalCmd ctx cmd of
      Left err -> putStrLn err >> return (Just ctx)
      Right (ctx',res) -> do
        continue <- case res of
          CmdResShow txt -> 
            putStrLn txt >> return True
          CmdResPrint txt ->
            putStrLn txt >> return True
          CmdResBound x Nothing ->
            putStrLn x >> return True
          CmdResBound x (Just t) -> 
            putStrLn (x ++ " = " ++ t) >> return True
          CmdResEmpty ->
            return True
          CmdResQuit ->
            return False
        if continue then return (Just ctx') else return Nothing
