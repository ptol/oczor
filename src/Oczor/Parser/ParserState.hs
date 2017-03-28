{-# LANGUAGE TemplateHaskell #-}
module Oczor.Parser.ParserState where
import Oczor.Syntax.Operators
import Control.Lens
import Text.Megaparsec.Expr
import Control.Monad.State
import ClassyPrelude as C hiding (try)
import Oczor.Syntax.Syntax
import qualified Text.Megaparsec.String as Megaparsec

type Parser = StateT ParserState Megaparsec.Parser

data ParserState = ParserState {
  _count :: Int,
  _asName :: Maybe String,
  _ops :: OperatorGroups,
  _opTable :: [[Operator Parser Expr]]
}

makeLenses ''ParserState

emptyState = ParserState {
  _count = 0,
  _asName = Nothing,
  _ops = [],
  _opTable = []
}

cleanAsName :: Parser ()
cleanAsName = asName .= Nothing

asNameOrFresh :: Parser String
asNameOrFresh = use asName >>= maybe freshName return

letters :: [String]
letters = [1..] >>= flip C.replicateM ['a'..'z']

freshName :: Parser String
freshName = do
  c <- use count
  count += 1
  return $ sysPrefix ++ unsafeIndex letters c
