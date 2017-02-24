{-# LANGUAGE PatternSynonyms       #-}
module Oczor.Syntax.Types where

import Data.Functor.Foldable hiding (Foldable)
import Oczor.Syntax.TypesF
import ClassyPrelude hiding (TVar)

typeBool  = TypeIdent "Bool"
typeUnit  = TypeIdent "Unit"
typeArray x =  TypeApply (TypeIdent "Array") [x]

type TypeExpr = Fix TypeExprF

type TVar = String
data Scheme = Forall [TVar] TypeExpr
  deriving (Show, Eq, Ord, Read)

type ConstraintSet = [(TypeExpr, ClassName)]

pattern TypeUnion x = Fix (TypeUnionF x)
pattern TypeIdent x = Fix (TypeIdentF x)
pattern TypePoly x y = Fix (TypePolyF x y)
pattern TypeVar x = Fix (TypeVarF x)
pattern TypeConstraints x y = Fix (TypeConstraintsF x y)
pattern TypeFunc x y = Fix (TypeFuncF x y)
pattern TypeLabel x y = Fix (TypeLabelF x y)
pattern TypeApply x y = Fix (TypeApplyF x y)
pattern NoType  = Fix NoTypeF 
pattern TypeRecord x = Fix (TypeRecordF x)
pattern TypeRow x y = Fix (TypeRowF x y)
