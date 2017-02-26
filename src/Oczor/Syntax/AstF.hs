{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell #-}
module Oczor.Syntax.AstF (module Oczor.Syntax.AstF) where

import ClassyPrelude
import Data.Deriving
import Data.Functor.Foldable
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
data ExprF e =
  LitF Lits |
  UniqObjectF String |
  WildCardF |
  IdentF Name |
  ParamIdentF Name |
  AsF (Maybe Name) e |
  CasesF [e] |
  LetF e e |
  SetStmtF e e |
  RecordLabelF Name e |
  DestructF e e |
  LabelAccessF Name |
  CallF e e |
  UpdateF e [e] |
  ExprListF [e] |
  RecordF [e] |
  FunctionF e (Maybe e) e |
  TypeDeclF Name TypeExpr |
  ClassFnF Name TypeExpr |
  InstanceFnF TypeExpr String e |
  FfiF Name TypeExpr |
  FfiTypeF Name TypeExpr |
  ExprTypeF TypeExpr |
  GetInstanceF Name TypeExpr |
  ArrayF [e] |
  WithTypeF e TypeExpr |
  InF Bool e |
  StmtF Stmts |
  IfF e e e |
  MDF AstPosition e
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

deriveEq1 ''ExprF
deriveOrd1 ''ExprF
deriveRead1 ''ExprF
deriveShow1 ''ExprF

instance Show (Fix ExprF) where showsPrec p (Fix x) = showsPrec p x
