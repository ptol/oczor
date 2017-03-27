{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Oczor.Syntax.Types where

import ClassyPrelude hiding (TVar)
import Data.Functor.Foldable.TH

data TypeExpr =
  TypeIdent String |
  TypeVar String |
  TypeUnion [TypeExpr] |
  TypePoly [TypeExpr] TypeExpr |
  TypeConstraints [(TypeExpr, String)] TypeExpr |
  TypeFunc TypeExpr TypeExpr |
  TypeLabel String TypeExpr |
  TypeApply TypeExpr [TypeExpr] |
  NoType |
  TypeRecord [TypeExpr] |
  TypeRow TypeExpr [TypeExpr]
  deriving (Eq, Ord, Show, Read)

makeBaseFunctor ''TypeExpr

typeBool  = TypeIdent "Bool"
typeUnit  = TypeIdent "Unit"
typeArray x =  TypeApply (TypeIdent "Array") [x]

type ClassName = String
type ConstraintSet = [(TypeExpr, ClassName)]

type TVar = String
data Scheme = Forall [TVar] TypeExpr
  deriving (Show, Eq, Ord, Read)
