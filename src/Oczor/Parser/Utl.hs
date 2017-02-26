module Oczor.Parser.Utl where

import Text.Megaparsec hiding (label)
import qualified Oczor.Parser.Lexer as L
import ClassyPrelude as C hiding (try)
import Oczor.Syntax.Syntax
import Text.Megaparsec.Expr as Ex
import Oczor.Parser.ParserState
import Oczor.Utl

toAstPosition p = (unPos $ sourceLine p, unPos $ sourceColumn p, sourceName p)

getAstPosition :: Parser AstPosition
getAstPosition = toAstPosition <$> getPosition

getPosFromError x = errorPos x & headEx & toAstPosition

md x = newMD <$> getAstPosition <*> x

opGroupsToTable :: OperatorGroups -> [[Operator Parser Expr]]
opGroupsToTable ops = ops & map (map infoToOperator)

infixToCall op = try (L.rop (op ^. opIdent)) *> return (\x y -> Call (Ident $ op ^. opFunc) (Record [x,y]))
opToCall op = try (L.rop (op ^. opIdent)) *> return (Call (Ident $ op ^. opFunc))
infoToOperator op =
  case op ^. assoc of
    OpInfixL -> InfixL $ infixToCall op
    OpInfixR -> InfixR $ infixToCall op
    OpInfixN -> InfixN $ infixToCall op
    OpPrefix -> Prefix $ opToCall op
    OpPostfix -> Postfix $ opToCall op

addOperators opGroups state = state & ops .~ operators & opTable .~ table
  where
    operators = unionOperatorGroups opGroups (state ^. ops)
    table = opGroupsToTable operators

anyOperatorParser = do
  L.ros & traverse_ (notFollowedBy . L.rop)
  x <- L.identOp
  fail $ "operator (" ++ x ++ ") is not defined"

defOperators :: [[Operator Parser Expr]]
defOperators =
  [
   -- [Ex.InfixL ((\x (ExprType y) -> WithType x y) <$ L.rop "::")],
   [Ex.InfixL (flip Call <$ L.rop "#")],
   [Ex.InfixR (Call <$ L.rop "$")],
   [Ex.InfixN anyOperatorParser]
  ]

recordIndentWith :: Parser Expr -> Parser Expr
recordIndentWith x = listToLetOrRecord <$> L.someIndent x

recordCommaWith item = listToLetOrRecord <$> L.commaSep1 item

applyWithType (RecordLabel rl body) (TypeLabel tl tp) | rl == tl = return $ RecordLabel rl (WithType body tp)
-- applyWithType (Record l) (TypeRecord tl) = Record (zip l tl & map (uncurry applyWithType))
-- applyWithType (Function p g b) (TypeFunc tp tb) = Function (applyWithType p tp) g (applyWithType b tb)
-- applyWithType (Let x y) tp = Let x (WithType y tp)
-- applyWithType x t = (WithType x t)
applyWithType x y = fail $ "cannot apply type " ++ show y ++ " to " ++ show x
