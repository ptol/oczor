{-# LANGUAGE TemplateHaskell #-}
module Oczor.Syntax.Operators where
import ClassyPrelude
import Control.Lens

data Assoc
  = OpInfixL
  | OpInfixR
  | OpInfixN
  | OpPrefix
  | OpPostfix
  deriving (Show, Eq, Read)

data OperatorInfo = OperatorInfo {
  _assoc :: Assoc,
  _precedence :: Int,
  _opIdent :: String,
  _opFunc :: String
} deriving (Eq, Show, Read)
newOperatorInfo a p op opf = OperatorInfo {_assoc = a, _precedence = p, _opIdent = op, _opFunc = opf}
makeLenses ''OperatorInfo

type OperatorGroups = [[OperatorInfo]]

unionOperatorGroups :: OperatorGroups -> OperatorGroups -> OperatorGroups
unionOperatorGroups l1 l2 = l1 ++ l2 & concat & sortOn (negate . view precedence) & groupAllOn (view precedence)
