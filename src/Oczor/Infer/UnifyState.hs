{-# LANGUAGE TemplateHaskell #-}
module Oczor.Infer.UnifyState where

import Oczor.Syntax.Syntax
import Oczor.Utl
import ClassyPrelude as C

import Oczor.Infer.State

data UnifyState = UnifyState {
  _constrains :: ConstrainSet,
  _openTypes :: Set String
} deriving (Eq, Show)

makeLenses ''UnifyState

initState = UnifyState {
  _constrains = mempty,
  _openTypes = mempty
}

type Unify = ReaderT UnifyState Infer

setGetConstrains :: String -> ConstrainSet -> [String]
setGetConstrains var x = x & filter (\(x,y) -> x == TypeVar var) & map snd

getConstrains :: String -> UnifyState -> [String]
getConstrains var state = state ^. constrains & setGetConstrains var

addConstrains list state  = state & constrains %~ concatNub list

removeConstrains :: String -> UnifyState -> UnifyState
removeConstrains var state = state & constrains %~ filter (\(x,y) -> x /= TypeVar var)
