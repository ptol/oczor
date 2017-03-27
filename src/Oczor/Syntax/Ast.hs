{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Oczor.Syntax.Ast (module Oczor.Syntax.Ast, module Oczor.Syntax.Types, Lits(..), Stmts(..)) where

import ClassyPrelude
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Oczor.Syntax.Types

type ModuleName  = [String]

data Lits =
  LitChar Char |
  LitBool Bool |
  LitDouble Double |
  LitInt Int |
  LitString String
  deriving (Eq, Ord, Show, Read)

data Stmts =
  StmtImport ModuleName (Maybe String) |
  StmtOpen String |
  StmtInclude ModuleName |
  StmtOperator
  deriving (Eq, Ord, Show, Read)

type AstPosition = (Word, Word, FilePath)

type Name = String

data Expr =
  Lit Lits |
  UniqObject String |
  WildCard |
  Ident Name |
  ParamIdent Name |
  As (Maybe Name) Expr |
  Cases [Expr] |
  Let Expr Expr |
  SetStmt Expr Expr |
  RecordLabel Name Expr |
  Destruct Expr Expr |
  LabelAccess Name |
  Call Expr Expr |
  Update Expr [Expr] |
  ExprList [Expr] |
  Record [Expr] |
  Function Expr (Maybe Expr) Expr |
  TypeDecl Name TypeExpr |
  ClassFn Name TypeExpr |
  InstanceFn TypeExpr String Expr |
  Ffi Name TypeExpr |
  FfiType Name TypeExpr |
  ExprType TypeExpr |
  GetInstance Name TypeExpr |
  Array [Expr] |
  WithType Expr TypeExpr |
  In Bool Expr |
  Stmt Stmts |
  If Expr Expr Expr |
  MD AstPosition Expr
  deriving (Eq, Ord, Show, Read)

makeBaseFunctor ''Expr

deriving instance Show a => Show (ExprF a)

data Ann f a = Ann (f (Ann f a)) a deriving (Functor, Foldable, Traversable)
data AnnF f a r = AnnF (f r) a deriving (Functor, Foldable, Traversable)

type instance Base (Ann f a) = AnnF f a

instance Functor f => Recursive (Ann f a) where
	project = \case Ann f a -> AnnF f a

instance Functor f => Corecursive (Ann f a) where
	embed = \case AnnF f a -> Ann f a

instance Show a => Show (Ann ExprF a) where
  show (Ann x y) = "(" ++ show x ++ " ANN " ++ show y ++ ")"

stripAnns :: Functor f => Ann f a -> Fix f
stripAnns = cata $ \case
  (AnnF x _) -> Fix x

attr :: Ann f a -> a
attr (Ann _ a) = a

unAnn :: Ann f a -> f (Ann f a)
unAnn (Ann a _) = a

changeAttr :: Ann f a -> a -> Ann f a
changeAttr (Ann x a) = Ann x

pattern UnAnn x <- Ann x y

pattern ExprListMD x <- MD y (ExprList x)
pattern LabelAccessCall label e = Call (LabelAccess label) e

