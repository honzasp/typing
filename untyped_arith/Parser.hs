module Parser(parseTerm, ParseError) where

import Text.Parsec
import Control.Applicative((<*), (<$>))
import Syntax

type Parser = Parsec [Char] ()

parseTerm :: String -> Either ParseError Term
parseTerm = parse term ""

term :: Parser Term
term =
  try atomicTerm <|>
  try ifTerm <|>
  try prefixTerm <|>
  (char '(' >> spaces >> term <* (spaces >> char ')'))

atomicTerm :: Parser Term
atomicTerm = true <|> false <|> zero
  where true = string "true" >> return TmTrue
        false = string "false" >> return TmFalse
        zero = string "0" >> return TmZero

ifTerm :: Parser Term
ifTerm = do
  string "if" >> many1 space
  t1 <- term
  many space >> string "then" >> many1 space
  t2 <- term
  many space >> string "else" >> many1 space
  t3 <- term
  return $ TmIf t1 t2 t3

prefixTerm :: Parser Term
prefixTerm = do
  con <- choice 
    [ string "succ" >> return TmSucc
    , string "pred" >> return TmPred
    , string "iszero" >> return TmIsZero ]
  many1 space
  con <$> term
