module Oczor.Infer.InferAst where

import ClassyPrelude
import Data.Functor.Foldable
import Oczor.Syntax.Syntax
import Oczor.Infer.Substitutable 
import Oczor.Infer.InferContext
import Oczor.Utl


type InferExprF = AnnF ExprF (TypeExpr, InferContext)
type InferExpr = Ann ExprF (TypeExpr, InferContext)

attrType :: Ann a (x, y) -> x
attrType = view (attr . _1)

annType x y = Ann x (y, emptyContext)

changeType newTp (Ann x (tp,ctx)) = Ann x (newTp, ctx)
changeContext newCtx (Ann x (tp,ctx)) = Ann x (tp, newCtx)

removeContext :: InferExpr -> Ann ExprF TypeExpr
removeContext = cata $ \case
  (AnnF x y) -> Ann x (fst y)

instance Substitutable InferExpr where
  -- apply s | traceArgs ["apply inferExpr", show s] = undefined
  apply s = cata $ \case (AnnF ast (tp,ctx)) -> Ann (apply s ast) (apply s tp, apply s ctx) -- TODO FF (apply ast)
  -- ftv = cata $ \case (AnnF ast tp) -> ftv tp 
