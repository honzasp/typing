{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Parser(InNameCtx, parseMod, parseCommand) where
import Control.Applicative((<*), (<*>), (<$>))
import Text.Parsec

import Command
import Context
import Term

type Parser = Parsec String ()

parseMod :: String -> Either String [Command]
parseMod txt = case parse (spaces >> modul <* eof) "" txt of
  Left err -> Left $ show err
  Right cmdsInNameCtx -> Right cmdsInNameCtx

parseCommand :: String -> Either String Command
parseCommand txt = case parse (spaces >> command <* eof) "" txt of
  Left err -> Left $ show err
  Right cmdInNameCtx -> Right cmdInNameCtx

modul :: Parser [Command]
modul = command `sepBy` symbol ";"

command :: Parser Command
command =
  try bindTermCmd <|> try special1Cmd <|> try special0Cmd <|>
  (CmdEvalTerm <$> try term) <|> return CmdEmpty

bindTermCmd = CmdBindTerm <$> (identifier <* symbol "=") <*> term
special0Cmd = CmdSpecial0 <$> (symbol ":" >> identifier)
special1Cmd = CmdSpecial1 <$> (symbol ":" >> identifier) <*> term

term :: Parser (InNameCtx Term)
term = try ifTerm <|> appsTerm

ifTerm = do
  t1 <- keyword "if" >> term
  t2 <- keyword "then" >> term
  t3 <- keyword "else" >> term
  return $ \ctx -> TmIf <$> t1 ctx <*> t2 ctx <*> t3 ctx

appsTerm = atomicTerm `chainl1` return app where
  app :: InNameCtx Term -> InNameCtx Term -> InNameCtx Term
  app t1 t2 = \ctx -> TmApp <$> t1 ctx <*> t2 ctx

atomicTerm =
  try trueTerm <|> try falseTerm <|> try zeroTerm <|>
  try succTerm <|> try predTerm <|> try iszeroTerm <|>
  try varTerm <|> try absTerm <|>
  try (between (symbol "(") (symbol ")") term)

trueTerm = keyword "true" >> con0 TmTrue
falseTerm = keyword "false" >> con0 TmFalse
zeroTerm = symbol "0" >> con0 TmZero

succTerm = keyword "succ" >> con1 TmSucc term
predTerm = keyword "pred" >> con1 TmPred term
iszeroTerm = keyword "iszero" >> con1 TmIszero term

varTerm = do
  x <- identifier
  return $ \ctx -> TmVar <$> ctxLookupIndex x ctx

absTerm = do
  x <- symbol "\\" >> identifier
  ty <- symbol ":" >> type_
  t <- symbol "." >> term
  return $ \ctx -> TmAbs x ty <$> t (ctxBind (x,NBndNameBind) ctx)

type_ :: Parser Type
type_ = atomicTy `chainr1` (try (symbol "->") >> return TyArr) where

atomicTy = boolTy <|> natTy
boolTy = keyword "Bool" >> return TyBool
natTy = keyword "Nat" >> return TyNat

con0 :: a -> Parser (InNameCtx a)
con0 con = return $ \ctx -> return con

con1 :: (a -> b) -> Parser (InNameCtx a) -> Parser (InNameCtx b)
con1 con p = do
  pInCtx <- p
  return $ \ctx -> con <$> pInCtx ctx

keyword :: String -> Parser ()
keyword kw = string kw >> notFollowedBy idChar >> spaces

symbol :: String -> Parser ()
symbol s = string s >> spaces

identifier :: Parser String
identifier = notKeyword >> ((:) <$> idStartChar <*> many idChar) <* spaces

notKeyword = notFollowedBy . choice $ map (try . keyword) keywords
keywords = 
  [ "true", "false", "if", "then", "else"
  , "succ", "pred", "iszero"
  , "Bool", "Nat"]

idStartChar = letter
idChar = alphaNum
