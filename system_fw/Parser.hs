module Parser(parseStmts) where
import Control.Applicative((<$>), (<$), (<*>), (<*))
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
stmt =
  stmtTermAbbr <|> stmtValBind <|> stmtTypeAbbr <|>
  stmtCmd <|> stmtEval <?> "statement"
  where
  stmtTermAbbr = StmtTermAbbr <$> try (identifier <* sym "=") <*> term
  stmtValBind = StmtValueBind <$> try (identifier <* sym "<-") <*> term
  stmtTypeAbbr = StmtTypeAbbr <$> try (identifier <* sym ":=") <*> ty
  stmtCmd = StmtCmd <$> cmd
  stmtEval = StmtEval <$> term

cmd :: Parser Command
cmd = trySym ":" >> choice
  [ tryWord "a" >> CmdAssert <$> term
  , tryWord "t" >> CmdType <$> term
  , tryWord "k" >> CmdKind <$> ty
  , tryWord "ctx" >> return CmdCtx
  , tryWord "dump" >> CmdDump <$> stmt
  , tryWord "dumpctx" >> return CmdDumpCtx
  , tryWord "resolved" >> CmdResolved <$> term
  , tryWord "q" >> return CmdQuit
  ] <?> "command"

term :: Parser (Term String)
term = term5 <?> "term" where
  term5 = termAbs <|> termTAbs <|> termIf <|> termCase <|> term4
  term4 = termAs
  term3 = termApps
  term2 = termProjs
  term1 = termBool <|> termUnit <|> termVar <|>
          termRcd <|> termVariant <|> paren term

  termAbs = TmAbs 
    <$> (trySym "\\" >> identifier)
    <*> (sym ":" >> ty)
    <*> (sym "." >> term5)

  termTAbs = TmTAbs
    <$> (trySym "/\\" >> identifier)
    <*> optionalKind
    <*> (sym "." >> term5)

  termIf = TmIf
    <$> (tryWord "if" >> term5)
    <*> (word "then" >> term5)
    <*> (word "else" >> term5)

  termCase = TmCase
    <$> between (tryWord "case") (word "of") term5
    <*> alt `sepBy` sym ","
  alt = (,,) 
    <$> (sym "<" >> identifier)
    <*> (sym "=" >> identifier)
    <*> (sym ">" >> sym "." >> term5)

  termAs = do
    t <- term3
    mbTy <- optionMaybe $ tryWord "as" >> ty
    case mbTy of
      Just ty -> return $ TmAs t ty
      Nothing -> return t

  -- TODO: get rid of `do`
  termApps = do
    hd <- term2
    tl <- many app
    return $ foldl (flip ($)) hd tl 
    where
    app = (flip TmApp) <$> term2 <|>
          (flip TmTApp) <$> between (trySym "[") (sym "]") ty

  termProjs = do
    hd <- term1
    tl <- many $ trySym "#" >> identifier
    return $ foldl TmProj hd tl

  termRcd = TmRcd <$> between (trySym "{") (sym "}") (field `sepBy` sym ",") where
    field = (,) <$> identifier <*> (sym "=" >> term)

  termVariant = TmVariant
    <$> (trySym "<" >> identifier)
    <*> (sym "=" >> term)
    <* sym ">"

  termVar = TmVar <$> try identifier
  termBool = TmTrue <$ tryWord "true" <|> TmFalse <$ tryWord "false"
  termUnit = TmUnit <$ tryWord "unit"

ty :: Parser (Type String)
ty = ty4 <?> "type" where
  ty4 = tyAll <|> tyAbs <|> ty3
  ty3 = tyArrs
  ty2 = tyApps
  ty1 = tyBool <|> tyUnit <|> tyVar <|> 
    tyRcd <|> tyVariant <|> paren ty

  tyAll = TyAll
    <$> (trySym "\\/" >> identifier)
    <*> optionalKind
    <*> (sym "." >> ty)

  tyAbs = TyAbs
    <$> (trySym "\\" >> identifier)
    <*> optionalKind
    <*> (sym "." >> ty)

  tyArrs = ty2 `chainr1` (TyArr <$ trySym "->")
  tyApps = ty1 `chainl1` (return TyApp)

  tyRcd = TyRcd <$> between (trySym "{") (sym "}") (field `sepBy` sym ",") where
    field = (,) <$> identifier <*> (sym "=" >> ty4)
  tyVariant = TyVariant <$> between (trySym "<") (sym ">") (variant `sepBy` sym ",") where
    variant = (,) <$> identifier <*> (sym "=" >> ty4)

  tyBool = TyBool <$ tryWord "Bool"
  tyUnit = TyUnit <$ tryWord "Unit"
  tyVar = TyVar <$> try identifier

kind :: Parser Kind
kind = kind2 <?> "kind" where
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
identifier = id >>= notKeyword <?> "identifier" where
  id = (:) <$> idStartChar <*> many idChar <* spaces
  notKeyword w = if w `elem` keywords
    then unexpected $ "keyword `" ++ w ++ "`"
    else return w
  keywords = ["if", "then", "else", "as", "case", "of"]

idStartChar, idChar :: Parser Char
idStartChar = letter <|> oneOf "_"
idChar = alphaNum <|> oneOf "_'"
