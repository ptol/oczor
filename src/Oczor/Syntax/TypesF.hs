{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE OverlappingInstances       #-}
module Oczor.Syntax.TypesF where
import ClassyPrelude
import Data.Functor.Foldable hiding (Foldable)
import Data.Functor.Classes

type ClassName = String
type ConstraintSetF e = [(e, ClassName)]

data TypeExprF e =
  TypeIdentF String |
  TypeVarF String |
  TypeUnionF [e] |
  TypePolyF [e] e |
  TypeConstraintsF (ConstraintSetF e) e |
  TypeFuncF e e |
  TypeLabelF String e |
  TypeApplyF e [e] |
  NoTypeF |
  TypeRecordF [e] |
  TypeRowF e [e]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Eq1 TypeExprF where
	eq1 = (==)
instance Ord1 TypeExprF where
	compare1 = compare
instance Read1 TypeExprF

instance Show (Fix TypeExprF) where showsPrec p (Fix x) = showsPrec p x
