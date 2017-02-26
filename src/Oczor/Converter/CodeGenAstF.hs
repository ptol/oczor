{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell #-}
module Oczor.Converter.CodeGenAstF where
import ClassyPrelude
import Data.Deriving
import Data.Functor.Classes
import Data.Functor.Foldable

type Name = String
data Lits =
  LitNull |
  LitBool Bool |
  LitChar Char |
  LitDouble Double |
  LitInt Int |
  LitString String
  deriving (Eq, Ord, Show)
data AstF e =
  NoneF |
  LitF Lits |
  UniqObjectF String |
  IdentF Name |
  NotEqualF e e |
  OperatorF String [e] |
  EqualF e e|
  VarF Name e |
  SetF e e |
  ThrowF String |
  ScopeF [e] e |
  StmtListF [e]|
  BoolAndsF [e]|
  ArrayF [e] |
  ReturnF e |
  HasFieldF e Name |
  LabelF Name e |
  FieldF e Name |
  ConditionOperatorF e e e |
  CodeF String |
  CallF e [e] |
  ParensF e |
  IfF e [e] [e] |
  ObjectF [(Name, e)] |
  FunctionF [String] [e]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


deriveEq1 ''AstF
deriveOrd1 ''AstF
deriveShow1 ''AstF

instance Show (Fix AstF) where showsPrec p (Fix x) = showsPrec p x
  -- show (Fix f) = show f

  -- show (Fix f) = "(" ++ show f ++ ")"
