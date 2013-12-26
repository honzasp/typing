module Parser(parseStmts) where
import Control.Applicative((<$>), (<$), (<*>), (<*), (*>))
import qualified Data.Maybe as M
import Text.Parsec

import Syntax

parseStmts :: String -> String -> Either String [Stmt]
parseStmts srcName src = case parse (between spaces eof stmts) srcName src of
  Left err -> Left $ show err
  Right stmts -> Right stmts

type Parser = Parsec String ()

stmts :: Parser [Stmt]
stmts = M.catMaybes <$> optionMaybe stmt `sepBy` sym "."

stmt :: Parser Stmt
stmt = stmtTermAbbr <|> stmtTypeAbbr <|> stmtCmd <|> stmtEval where
  stmtTermAbbr = StmtTermAbbr <$> try (identifier <* sym "=") <*> term
  stmtTypeAbbr = StmtTypeAbbr <$> try (identifier <* sym ":=") <*> ty
  stmtCmd = StmtCmd <$> cmd
  stmtEval = StmtEval <$> term

cmd :: Parser Command
cmd = trySym "!" >> choice
  [ tryWord "a" >> CmdAssert <$> term
  , tryWord "t" >> CmdType <$> term
  , tryWord "k" >> CmdKind <$> ty
  , tryWord "q" >> return CmdQuit
  ]

term :: Parser (Term String)
term = term3 where
  term3 = termAbs <|> termTAbs <|> termIf <|> term2
  term2 = termApps
  term1 = termVar <|> termBool <|> termUnit <|> paren term

  termAbs = TmAbs 
    <$> (trySym "\\" >> identifier)
    <*> (sym ":" >> ty)
    <*> (sym "." >> term3)

  termTAbs = TmTAbs
    <$> (trySym "/\\" >> identifier)
    <*> optionalKind
    <*> (sym "." >> term3)

  termIf = TmIf
    <$> (tryWord "if" >> term3)
    <*> (word "then" >> term3)
    <*> (word "else" >> term3)

  -- TODO: get rid of `do`
  termApps = do
    hd <- term1
    tl <- many app
    return $ foldl (flip ($)) hd tl

  app = (flip TmApp) <$> term <|>
        (flip TmTApp) <$> (trySym "[" *> ty <* sym "]")

  termVar = TmVar <$> try identifier
  termBool = TmTrue <$ tryWord "true" <|> TmFalse <$ tryWord "false"
  termUnit = TmUnit <$ tryWord "unit"

ty :: Parser (Type String)
ty = ty4 where
  ty4 = tyAll <|> tyAbs <|> ty3
  ty3 = tyArrs
  ty2 = tyApps
  ty1 = tyBool <|> tyUnit <|> tyVar <|> paren ty

  tyAll = TyAll
    <$> (trySym "\\/" >> identifier)
    <*> optionalKind
    <*> (sym "." >> ty)

  tyAbs = TyAbs
    <$> (trySym "\\" >> identifier)
    <*> optionalKind
    <*> (sym "." >> ty)

  tyArrs = ty2 `chainr1` (TyArr <$ trySym "->")
  tyApps = ty1 `chainr1` (return TyApp)

  tyBool = TyBool <$ tryWord "Bool"
  tyUnit = TyUnit <$ tryWord "Unit"
  tyVar = TyVar <$> try identifier

kind :: Parser Kind
kind = kind2 where
  kind2 = kindApps
  kind1 = kindStar <|> paren kind

  kindApps = kind1 `chainr1` (KiArr <$ trySym "->")
  kindStar = KiStar <$ trySym "*"

optionalKind :: Parser Kind
optionalKind = option KiStar (trySym "::" >> kind)

paren :: Parser a -> Parser a
paren = between (trySym "(") (sym ")")

sym, word :: String -> Parser ()
sym s = string s >> spaces
word w = string w >> notFollowedBy idChar >> spaces

trySym, tryWord :: String -> Parser ()
trySym = try . sym
tryWord = try . word

identifier :: Parser String
identifier = (:) <$> idStartChar <*> many idChar

idStartChar, idChar :: Parser Char
idStartChar = letter
idChar = alphaNum
