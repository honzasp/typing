{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Parser(InNameCtx, parseMod, parseCommand) where
import Control.Applicative((<*), (*>), (<*>), (<$>))
import Data.Maybe(catMaybes)
import Text.Parsec

import Command
import Context
import Term

type Parser = Parsec String ()

parseMod :: String -> Either String [Command]
parseMod txt = case parse (spaces *> modul <* eof) "" txt of
  Left err -> Left $ show err
  Right cmdsInNameCtx -> Right cmdsInNameCtx

parseCommand :: String -> Either String Command
parseCommand txt = case parse (spaces *> command <* eof) "" txt of
  Left err -> Left $ show err
  Right cmdInNameCtx -> Right cmdInNameCtx

modul :: Parser [Command]
modul = catMaybes <$> optionMaybe command `sepBy` symbol ";"

command :: Parser Command
command = bindTermCmd <|> specialCmd <|> evalTermCmd

bindTermCmd = CmdBindTerm <$> (identifier <* symbol "=") <*> try term
evalTermCmd = CmdEvalTerm <$> try term

specialCmd = symbol ":" >> CmdSpecial <$> choice (map try cmds) 
  where cmds = [quitCmd, typeCmd, assertCmd, ppCmd, dbgParsedCmd]

quitCmd = keyword "q" >> return CmdSpecQuit
typeCmd = keyword "t" >> CmdSpecType <$> term
assertCmd = keyword "a" >> CmdSpecAssert <$> term
ppCmd = keyword "pp" >> CmdSpecPrettyPrint <$> term
dbgParsedCmd = keyword "_p" >> CmdSpecDbgParsed <$> term

term :: Parser (InNameCtx Term)
term = term4

term4 = letTerm <|> ifTerm <|> fixTerm <|> absTerm <|> term3
term3 = appsTerm
term2 = succTerm <|> predTerm <|> iszeroTerm <|> term1
term1 = projsTerm
term0 = trueTerm <|> falseTerm <|> unitTerm <|>
  natTerm <|> varTerm <|> tupleTerm <|>
  between (symbol "(") (symbol ")") term

ifTerm = do
  t1 <- keyword "if" >> term4
  t2 <- keyword "then" >> term4
  t3 <- keyword "else" >> term4
  return $ \ctx -> TmIf <$> t1 ctx <*> t2 ctx <*> t3 ctx

letTerm = do
  x <- keyword "let" >> identifier
  t1 <- symbol "=" >> term4
  t2 <- keyword "in" >> term4
  return $ \ctx -> TmLet x <$> t1 ctx <*> t2 (ctxBind (x,NBndNameBind) ctx)

absTerm = do
  x <- symbol "\\" >> identifier
  ty <- symbol ":" >> type_
  t <- symbol "." >> term4
  return $ \ctx -> TmAbs x ty <$> t (ctxBind (x,NBndNameBind) ctx)

fixTerm = keyword "fix" >> con1 TmFix <$> term4

appsTerm = do
  t1 <- term2
  t2s <- many $ try term1
  return $ \ctx -> foldApps <$> t1 ctx <*> mapM ($ ctx) t2s
  where foldApps f ts = foldl TmApp f ts

projsTerm = do
  t1 <- term0
  is <- many $ try (symbol "." *> proj)
  return $ \ctx -> foldProjs is <$> t1 ctx
  where proj = fromIntegral <$> natural
        foldProjs is term = foldl TmProj term is

trueTerm = keyword "true" >> con0 TmTrue
falseTerm = keyword "false" >> con0 TmFalse
unitTerm = keyword "unit" >> con0 TmUnit

succTerm = keyword "succ" >> con1 TmSucc <$> term1
predTerm = keyword "pred" >> con1 TmPred <$> term1
iszeroTerm = keyword "iszero" >> con1 TmIszero <$> term1

natTerm = do
   n <- natural
   return $ \ctx -> return $ TmNat n

tupleTerm = wrapTuple <$> members
  where members = between (symbol "{") (symbol "}") (term `sepBy` try (symbol ","))
        wrapTuple ts ctx = TmTuple <$> mapM ($ ctx) ts

varTerm = do
  x <- identifier
  return $ \ctx -> TmVar <$> ctxLookupIndex x ctx

type_ :: Parser Type
type_ = atomicTy `chainr1` (try (symbol "->") >> return TyArr) where

atomicTy = boolTy <|> natTy <|> unitTy
boolTy = keyword "Bool" >> return TyBool
natTy = keyword "Nat" >> return TyNat
unitTy = keyword "Unit" >> return TyUnit

con0 :: a -> Parser (InNameCtx a)
con0 con = return $ \ctx -> return con

con1 :: (a -> b) -> InNameCtx a -> InNameCtx b
con1 f inCtxA = \ctx -> f <$> inCtxA ctx

natural :: Parser Integer
natural = read <$> many1 digit <* spaces

keyword :: String -> Parser ()
keyword kw = try $ string kw >> notFollowedBy idChar >> spaces

symbol :: String -> Parser ()
symbol s = string s >> spaces

identifier :: Parser String
identifier = notFollowedBy keyword >> ((:) <$> idStartChar <*> many idChar) <* spaces
  where keyword = choice $ map (try . string) keywords
        keywords = 
          [ "true", "false", "unit"
          , "if", "then", "else"
          , "succ", "pred", "iszero"
          , "let", "in"
          , "fix"
          , "Bool", "Nat", "Unit"]

idStartChar = letter
idChar = alphaNum
