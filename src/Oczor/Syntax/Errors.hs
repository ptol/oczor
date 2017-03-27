module Oczor.Syntax.Errors where
import Oczor.Syntax.Ast
import ClassyPrelude hiding (TVar)

type Error = (ErrorType, AstPosition)

data ErrorType =
  ParserError String |
  UnificationFail TypeExpr TypeExpr |
  InfiniteType String TypeExpr |
  TextError String |
  CircularDependency [ModuleName] |
  NoInstance TypeExpr String |
  UnboundVariable String |
  UnboundModule ModuleName |
  ModuleNotExists ModuleName |
  UnboundType String |
  UnboundClass String |
  TypeUnionWithUnion String |
  UnificationMismatch [TypeExpr] [TypeExpr]
  deriving (Eq, Ord, Show)

isNoInstance = \case
  NoInstance {} -> True
  _ -> False
