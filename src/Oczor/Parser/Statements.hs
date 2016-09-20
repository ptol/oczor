module Oczor.Parser.Statements where

import Text.Megaparsec hiding (label)



import qualified Oczor.Parser.Lexer as L
import ClassyPrelude as C hiding (try)
import Oczor.Syntax.Syntax

import Control.Monad.State

import Oczor.Parser.ParserState
import Oczor.Parser.Utl
import Oczor.Parser.Types
import Oczor.Parser.Expr



stmts = Stmt <$> (stmtOperator <|> stmtOpen) <|> stmtInclude  <|> stmtImport

stmt = md $ stmtSet <|> stmts <|> typeDecl <|> ffiType <|> ffi <|> classFn <|> instanceFn <|> labelWithType <|> typeDef

moduleName = sepBy1 L.ident (string ".")

stmtInclude :: Parser Expr
stmtInclude = keywordList "include" includeBody where
  includeBody = Stmt . StmtInclude <$> moduleName

stmtImport = keywordList "import" importBody where
  importBody :: Parser Expr
  importBody = Stmt <$> (StmtImport <$> moduleName <*> asName)
  asName = optional (try $ L.rword "as" *> L.ident)

stmtOperator :: Parser Stmts
stmtOperator = do
  ac <-
    (try ( L.rword "infixl") *> return OpInfixL) <|>
    (try ( L.rword "infixr") *> return OpInfixR) <|>
    (try ( L.rword "infix") *> return OpInfixN) <|>
    (try ( L.rword "prefix") *> return OpPrefix) <|>
    (try ( L.rword "postfix") *> return OpPostfix)
  ident <- L.identOp
  prec <- L.litInt
  func <- L.ident
  let info = newOperatorInfo ac (fromInteger prec) ident func
  modify (addOperators [[info]])
  return StmtOperator


stmtOpen :: Parser Stmts
stmtOpen = StmtOpen <$> (try (L.rword "open") *> L.ident)

typeDef :: Parser Expr
typeDef = do
  typeLbl <- typeLabel
  next <- optional (L.scn *> (try func <|> label))
  maybe (return $ ExprType typeLbl) (flip applyWithType typeLbl) next

-- class

keywordList keyword line = try (L.rword keyword) *> (listIfSome <$> L.someIndent line)



classFn :: Parser Expr
classFn = keywordList "class" classBody where
  classBody :: Parser Expr
  classBody = do
    name <- L.ident
    var <- L.ident
    L.rop ":"
    body <- typeRecord
    return $ ClassFn name (TypePoly [TypeVar var] body)

instanceFn :: Parser Expr
instanceFn = keywordList "instance" instanceBody where
  instanceBody :: Parser Expr
  instanceBody = do
    tp <- L.parens typeItem <|> typeIdent
    MD _ (RecordLabel name fn) <- record
    return $ InstanceFn tp name fn

ffi :: Parser Expr
ffi = Ffi <$> (try ( L.rword "ffi") *> L.ident <* L.rop ":") <*> typeRecord

ffiType :: Parser Expr
ffiType = do
  name <- try (L.rword "ffi" *> L.rword "type") *> L.identType
  param <- many typeParam
  let body = TypeIdent name
  return $ FfiType name (if onull param then body else TypePoly param body)

