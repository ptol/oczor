module Oczor.Parser.Expr where

import Text.Megaparsec hiding (label)
import qualified Oczor.Parser.Lexer as L
import ClassyPrelude as C hiding (try)
import Oczor.Syntax.Syntax
import Text.Megaparsec.Expr as Ex
import Control.Monad.State
import qualified Oczor.Desugar.Desugar as Desugar
import Oczor.Parser.ParserState
import Oczor.Parser.Utl
import Oczor.Parser.Types

import Oczor.Utl

litInt :: Parser Expr
litInt = Lit . LitInt . fromInteger <$> L.litInt

litChar :: Parser Expr
litChar = Lit . LitChar <$> L.litChar

litBool :: Parser Expr
litBool = Lit . LitBool <$> try ((L.rword "true" *> return True) <|> (L.rword "false" *> return False))

litStr :: Parser Expr
litStr = Lit . LitString <$> L.litStr

litDouble :: Parser Expr
litDouble = Lit . LitDouble <$> try L.litFloat

lits = litBool <|> litChar <|> litStr <|> litDouble <|> litInt

wildcard :: Parser Expr
wildcard = L.rword "_" *> return WildCard

stmtSet :: Parser Expr
stmtSet = do
  expr <- try (l <* L.rop ":=")
  value <- record
  return $ SetStmt expr value
  where
    l = labelAccess <|> ident
  
recordItem = stmtSet <|> try destruct <|> expr

record :: Parser Expr
record = recordIndentWith (recordCommaWith recordItem)

  
recordWith :: Parser Expr ->  Parser Expr
recordWith x = recordIndentWith (recordCommaWith x)

exprItemCommon = ifExpr <|> cases <|> labelAccess <|> lits <|> ident <|> array

exprItemWith letExprParser = md $ try (L.parens record) <|> letExprParser <|>
  update <|> try func <|> anonFunc <|> try call <|>
  labelWith (recordIndentWith exprLabel) <|> exprItemCommon

exprItem = exprItemWith letExpr

argExpr = md $ L.parens exprRecord <|> letExpr <|> labelWith record <|> exprItemCommon <|> wildcard

exprWith :: Parser Expr -> Parser Expr
exprWith item = do
  s <- get
  let table = (s ^. opTable) ++ defOperators
  Ex.makeExprParser item table

  

exprLabel :: Parser Expr
exprLabel = exprWith exprItem

expr :: Parser Expr
expr = md $ exprWith $ recordCommaWith exprItem

guardArgItem = md $ L.parens (try call <|> anonFunc <|> exprRecord) <|> labelWith expr <|> exprItemCommon
guardArg :: Parser Expr
guardArg = exprWith guardArgItem

exprRecord = recordCommaWith expr

labelAccess = do
  (MD p e) <- try (body <* char '.')
  labels <- sepBy1 L.ident (char '.')
  MD p <$> C.foldM newLabelAccess (unwrapMD e) labels
  where
    newLabelAccess x label = Desugar.partialApply $ Call (LabelAccess label) x
    body = md $ ident <|> wildcard <|> L.parens expr

label :: Parser Expr
label = labelWith record

labelType :: Parser (Maybe TypeExpr)
labelType = optional (L.rop ":" *> typeRecord)

labelWith :: Parser Expr -> Parser Expr
labelWith body = RecordLabel <$> try (L.ident <* L.rop "=") <*> body


labelWithType :: Parser Expr
labelWithType = do
  (lbl,tp) <- try (((,) <$> (L.ident <* L.rop ":") <*> typeRecord) <* L.rop "=")
  b <- record
  return $ RecordLabel lbl (WithType b tp)

destruct :: Parser Expr
destruct = do
  left <- try (L.parens destructRecord) <* L.rop "="
  right <- record
  Desugar.destruct $ Destruct left right
  where
    destructItem = L.parens destructRecord <|> labelWith destructItem <|> ident
    destructRecord = recordIfSome <$> L.commaSep1 destructItem


ident :: Parser Expr
ident = Ident <$> L.ident

update :: Parser Expr
update = do
  id <- try (ident <* L.rword "with")
  labels <- recordToList <$> record -- L.parens (L.commaSep1 label) <|> ((:[]) <$> label)
  return $ Update id labels

ifExpr :: Parser Expr
ifExpr = do
  b <- try (L.rword "if" *> record)
  t <- L.rword "then" *> record
  f <- L.rword "else" *> record
  return $ If b t f

array :: Parser Expr
array = do
  var <- try simpleItems <|> recordItems
  Desugar.partialApply $ Array var
  where
    simpleItem = md $ L.parens record <|> labelAccess <|> lits <|> ident <|> wildcard
    simpleItems = L.brackets $ many simpleItem
    recordItems = recordToList <$> L.brackets record

recordIfSomeComma x = recordIfSome <$> L.commaSep1 x

funcParam :: Parser Expr
funcParam = recordIfSomeExceptRecord <$> some paramRecordComma
  where
    paramLabel :: Parser Expr
    paramLabel = labelWith paramItem

    paramIdent :: Parser Expr
    paramIdent = ParamIdent <$> L.ident

    identOut :: Parser Expr
    identOut = Ident <$> L.identOut

    asParamExpr :: Parser Expr
    asParamExpr = do
      name <- try (optional L.ident <* L.rop "@")
      param <- paramItem
      return $ As name param
    paramItemAny = asParamExpr <|> lits <|> wildcard <|> identOut <|> paramIdent
    paramParens = (ExprType <$> typeLabel) <|> paramLabel <|> paramItemAny
    paramItem :: Parser Expr
    paramItem = md $ L.parens (recordIfSomeComma paramParens) <|> paramItemAny

    paramRecordComma :: Parser Expr
    paramRecordComma = try (L.parens paramRecordComma) <|> recordIfSomeComma paramItem

anonFuncParamGuard :: Parser (Expr, Maybe Expr)
anonFuncParamGuard = ((,) <$> (backslash *> funcParam) <*> funcGuard) <* L.rop "=>" where
  backslash = option () (void $ L.rop "\\")
  funcGuard :: Parser (Maybe Expr)
  funcGuard = optional (L.rop "|" *> guardArg)

newFunction param guard body = Desugar.func $ Function param guard body

func :: Parser Expr
func = do
  (name, params) <- try (((,) <$> L.ident <*> funcParam) <* L.rop "=")
  body <- record
  RecordLabel name <$> newFunction params Nothing body
anonFunc = do
  x <- anonFuncRaw
  Desugar.func x
anonFuncRaw :: Parser Expr
anonFuncRaw = do
  (param, guard) <- try anonFuncParamGuard
  body <- record
  return $ Function param guard body

anonFuncSingleParam :: Parser Expr
anonFuncSingleParam = do
  (param, guard) <- try anonFuncParamGuard
  body <- record
  Desugar.funcSingleParam $ Function param guard body


call :: Parser Expr
call = do
  name <- ident <|> L.parens record
  args <- try (listToLetOrRecord <$> some argExpr)
  Desugar.partialApply $ Call name args

desugarCases [x] = (Cases . (: [])) <$> Desugar.func x
desugarCases list =
  if olength arities == 1 then Cases <$> (list &traverse Desugar.func)
  else Cases <$> (list & traverse Desugar.funcSingleParam)
  where
    arities = casesArity list

cases :: Parser Expr
cases = do
 body <- try (L.rword "case" *> (try (some $ L.parens anonFuncRaw) <|> L.someIndent anonFuncRaw))
 desugarCases body

letExpr = do
  l <- try (letKw *> recordWith (labelWith exprLet) <* L.rword "in")
  r <- record
  return $ Let l r
  where
    letKw = option () (void $ L.rword "let")
    exprLet :: Parser Expr
    exprLet = exprWith (exprItemWith mzero)
