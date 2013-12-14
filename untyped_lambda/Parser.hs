module Parser(InCtx, parseMod, parseCommand) where
import Control.Applicative((<*), (<*>), (<$>), liftA2)
import Data.Char
import Text.Parsec

import Syntax

type Parser = Parsec String ()

parseMod :: String -> Either String [Command]
parseMod txt = case parse (spaces >> modul <* eof) "" txt of
  Left err -> Left $ show err
  Right cmdsInCtx -> Right cmdsInCtx

parseCommand :: String -> Either String Command
parseCommand txt = case parse (spaces >> command <* eof) "" txt of
  Left err -> Left $ show err
  Right cmdInCtx -> Right cmdInCtx

modul :: Parser [Command]
modul = command `sepBy` symbol ';'

command :: Parser Command
command = try bindNameCmd <|> try bindTermCmd <|> try specialCmd
  <|> (CmdEvalTerm <$> try term) <|> return CmdEmpty

bindNameCmd = CmdBindName <$> (identifier <* symbol '/')
bindTermCmd = CmdBindTerm <$> (identifier <* symbol '=') <*> term
specialCmd = CmdSpecial <$> (symbol ':' >> identifier)

term :: Parser (InCtx Term)
term = atomicTerm `chainl1` (return app) where
  app :: InCtx Term -> InCtx Term -> InCtx Term
  app t1 t2 = \ctx -> TmApp <$> t1 ctx <*> t2 ctx

atomicTerm :: Parser (InCtx Term)
atomicTerm = try varTerm <|> try absTerm <|>
  try (between (symbol '(') (symbol ')') term)

varTerm :: Parser (InCtx Term)
varTerm = do
  x <- identifier
  return $ (TmVar <$>) . ctxLookupIndex x

absTerm :: Parser (InCtx Term)
absTerm = do
  x <- symbol '\\' >> identifier
  t <- symbol '.' >> term
  return $ \ctx -> TmAbs x <$> t (ctxBind x NameBind ctx)

identifier :: Parser String
identifier = ((++) <$> many1 letter <*> many alphaNum) <* spaces

symbol :: Char -> Parser ()
symbol s = char s >> spaces
