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
typeUnion = TypeUnion <$> try (L.sepBy2 typeUnionItem (L.rop "|"))

typeConstrain :: Parser (String, [String])
typeConstrain = unsafeHead . C.uncons <$> try (some L.ident)

typeConstraints :: Parser TypeExpr
typeConstraints = do
  list <- try (L.commaSep1 typeConstrain <* L.rop "<:")
  tp <- typeItem
  return $ TypeConstraints (separateVarConstraints list) tp
  where
    separateVarConstraints list = list >>= (\(x, y) -> y &map (\y -> (TypeVar x,y)))

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

typeUnionItem = choice
  [L.parens typeRecord,
  typeConstraints,
  try typeApplyParam,
  typeLabel,
  typeVar,
  typeIdent]

typeItem = typeUnion <|> typeUnionItem

typeArg = choice
  [L.parens typeRecord,
  typeConstraints,
  typeLabel,
  typeVar,
  typeIdent]

typeIdent :: Parser TypeExpr
typeIdent = TypeIdent <$> L.identType

typeIdentWithConstains :: Parser TypeExpr
typeIdentWithConstains = TypeIdent <$> L.identType

typeLabelWith :: Parser String -> Parser TypeExpr
typeLabelWith x = liftA2 TypeLabel
  (try (x <* L.rop ":" <* notFollowedBy (L.rop "=")))
  (typeFunc <|> typeUnionItem)
 

typeLabel :: Parser TypeExpr
typeLabel = typeLabelWith L.ident

typeRecord = newTypeRecord <$> L.commaSep1 (typeFunc <|> typeItem)

typeRecordComma = newTypeRecord <$> L.commaSep1 typeItem

typeRecordIdent = newTypeRecord <$> L.indentOrComma typeItem

typeParam :: Parser TypeExpr
typeParam = TypeVar <$> L.ident
