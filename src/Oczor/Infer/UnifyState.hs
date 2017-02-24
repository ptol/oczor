{-# LANGUAGE TemplateHaskell #-}
module Oczor.Infer.UnifyState where

import Oczor.Syntax.Syntax
import Oczor.Utl
import ClassyPrelude as C

import Oczor.Infer.State

data UnifyState = UnifyState {
  _constraints :: ConstraintSet,
  _openTypes :: Set String
} deriving (Eq, Show)

makeLenses ''UnifyState

initState = UnifyState {
  _constraints = mempty,
  _openTypes = mempty
}

type Unify = ReaderT UnifyState Infer

setGetConstraints :: String -> ConstraintSet -> [String]
setGetConstraints var x = x & filter (\(x,y) -> x == TypeVar var) & map snd

getConstraints :: String -> UnifyState -> [String]
getConstraints var state = state ^. constraints & setGetConstraints var

addConstraints list state  = state & constraints %~ concatNub list

removeConstraints :: String -> UnifyState -> UnifyState
removeConstraints var state = state & constraints %~ filter (\(x,y) -> x /= TypeVar var)
