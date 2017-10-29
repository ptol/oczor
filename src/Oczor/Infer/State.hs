{-# LANGUAGE TemplateHaskell #-}
module Oczor.Infer.State (Infer(..), freshVar, addSubst, applySubst, emptySubst, letters, renameVarsInType, instantiate, runInfer, fresh) where

import Oczor.Syntax.Syntax
import Oczor.Utl
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import ClassyPrelude as C

import Oczor.Infer.InferContext

import Oczor.Infer.Substitutable
import Oczor.Infer.InferAst

data InferState = InferState {
  _count :: Int,
  _inferSubst :: Subst
}

makeLenses ''InferState

initInfer :: InferState
initInfer = InferState {
  _count = 0,
  _inferSubst = emptySubst
}

addSubst :: Subst -> Infer ()
addSubst s = inferSubst %= composeSubst s

applySubst :: Substitutable a => a -> Infer a
applySubst t = do
  subst <- use inferSubst
  return $ apply subst t

letters :: [String]
letters = [1..] >>= flip C.replicateM ['a'..'z']


type Infer = ReaderT InferContext (StateT InferState (Except Error))

runInfer :: InferContext -> Infer (InferContext, InferExpr) -> Either Error (InferContext, InferExpr)
runInfer context m = evalStateT (runReaderT m context) initInfer & runExcept
-- runInfer context m = (runReader m context) initInfer & runExcept <&> fst

-- runInfer :: InferContext -> Infer (InferContext, InferExpr) -> _
-- runInfer context m = evalStateT (runReaderT m context) initInfer & runExcept

freshVar :: Infer String
freshVar = do
    c <- use count
    count += 1
    return (unsafeIndex letters c)

fresh :: Infer TypeExpr
fresh = TypeVar <$> freshVar

instantiate ::  Scheme -> Infer TypeExpr
instantiate (Forall as t) = renameVars as t

renameVars :: [String] -> TypeExpr -> Infer TypeExpr
renameVars vars tp = do
  m <- freshMapping vars
  return $ renameTypeVars m tp

freshMapping :: [String] -> Infer (Map String String)
freshMapping vars = mapFromList <$> mapM (\x -> (x,) <$> freshVar) vars

renameVarsInType :: TypeExpr -> Infer TypeExpr
-- renameVarsInType tp | traceArgs ["renameVarsInType", show tp]= undefined
renameVarsInType tp = do
  let vars = setToList $ ftv tp
  renameVars vars tp
