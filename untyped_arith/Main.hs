module Main(main) where
import Text.PrettyPrint(render)
import System.IO
import Syntax
import Eval
import Parser

main = mainLoop where
  mainLoop = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case parseTerm input of
      Left err -> putStrLn $ "Parse error: " ++ show err
      Right term -> do
        case eval term of
          Left err -> putStrLn $ "Error: " ++ err
          Right value -> putStrLn . render . ppValue $ value
    mainLoop
