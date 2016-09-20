module Oczor.Parser.Types where

import Text.Megaparsec hiding (label)
import qualified Oczor.Parser.Lexer as L
import ClassyPrelude as C hiding (try)
import Oczor.Syntax.Syntax
import qualified Oczor.Desugar.Desugar as Desugar
import Oczor.Parser.ParserState
import Oczor.Utl

typeDecl :: Parser Expr
typeDecl = do
  name <- try (L.rword "type") *> L.identType
  param <- many typeParam <* L.rop "="
  body <- typeRecordIdent
  return $ Desugar.typeDecl name param body

typeUnion :: Parser TypeExpr
typeUnion = do
  list <- try (L.sepBy2 typeUnionItem (L.rop "|"))
  return $ TypeUnion list


typeConstrain :: Parser (String, [String])
typeConstrain = do
  list <- try (some L.ident)
  return $ C.uncons list & unsafeHead

typeConstrains :: Parser TypeExpr
typeConstrains = do
  list <- try (L.commaSep1 typeConstrain <* L.rop "<:")
  tp <- typeItem
  return $ TypeConstrains (separateVarConstrains list) tp
  where
    separateVarConstrains list = list >>= (\(x, y) -> y &map (\y -> (TypeVar x,y)))

typeVar :: Parser TypeExpr
typeVar = TypeVar <$> L.ident

typeFunc :: Parser TypeExpr
typeFunc = do
  var <- try (typeRecordComma <* L.rop "=>")
  body <- typeItem
  return $ TypeFunc var body

typeApplyParam :: Parser TypeExpr
typeApplyParam = do
  name <- typeIdent <|> typeVar
  args <- some typeArg
  return $ TypeApply name args

typeUnionItem = 
  L.parens typeRecord <|>
  typeConstrains <|>
  try typeApplyParam <|>
  typeLabel <|>
  typeVar <|>
  typeIdent

typeItem = typeUnion <|> typeUnionItem

typeArg =
  L.parens typeRecord <|>
  typeConstrains <|>
  typeLabel <|>
  typeVar <|>
  typeIdent

typeIdent :: Parser TypeExpr
typeIdent = do
  var <- L.identType
  return $ TypeIdent var

typeIdentWithConstains :: Parser TypeExpr
typeIdentWithConstains = do
  var <- L.identType
  return $ TypeIdent var

typeLabelWith :: Parser String -> Parser TypeExpr
typeLabelWith x = do
  lbl <- try (x <* L.rop ":" <* notFollowedBy (L.rop "="))
  b <- typeFunc <|> typeUnionItem
  return $ TypeLabel lbl b

typeLabel :: Parser TypeExpr
typeLabel = typeLabelWith L.ident

typeRecord = newTypeRecord <$> L.commaSep1 (typeFunc <|> typeItem)

typeRecordComma = newTypeRecord <$> L.commaSep1 typeItem

typeRecordIdent = newTypeRecord <$> L.indentOrComma typeItem

typeParam :: Parser TypeExpr
typeParam = TypeVar <$> L.ident
