{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE OverlappingInstances       #-}
module Oczor.Syntax.Ast (module Oczor.Syntax.Ast, module Oczor.Syntax.Types, Lits(..), Stmts(..)) where

import ClassyPrelude
import Data.Functor.Foldable hiding (Foldable)
import Oczor.Syntax.AstF
import Oczor.Syntax.Types

type Expr = Fix ExprF


data AnnF f a r = AnnF (f r) a deriving (Show, Functor, Foldable)

instance Show a => Show (Ann ExprF a) where
  show (Fix (AnnF x y)) = "(" ++ show x ++ " ANN " ++ show y ++ ")"

type Ann f a = Fix (AnnF f a)

-- ann :: Expr -> TypeExpr -> InferExpr
-- ann :: f (Fix (AnnF f a)) -> a -> Fix (AnnF f a)
-- ann x y = Fix (AnnF x y)

ann x y = Fix $ AnnF x y

stripAnns :: Functor f => Ann f a -> Fix f
stripAnns = cata $ \case
  (AnnF x _) -> Fix x


attr :: Ann f a -> a
attr (unfix -> AnnF _ a) = a


unAnn :: Ann f a -> f (Ann f a)
unAnn (unfix -> AnnF a y) = a

changeAttr :: Ann f a -> a -> Ann f a
changeAttr (unfix -> AnnF x a) na = ann x na

pattern Ann x y = Fix (AnnF x y)
pattern UnAnn x <- Fix (AnnF x y)

pattern Lit x = Fix (LitF x)
pattern WildCard = Fix WildCardF
pattern UniqObject x = Fix (UniqObjectF x)
pattern Ident x = Fix (IdentF x)
pattern ParamIdent x = Fix (ParamIdentF x)
pattern As x y = Fix (AsF x y)
pattern Cases x = Fix (CasesF x)
pattern Let x y = Fix (LetF x y)
pattern RecordLabel x y = Fix (RecordLabelF x y)
pattern LabelAccess x = Fix (LabelAccessF x)
pattern Call x y = Fix (CallF x y)
pattern Update x y = Fix (UpdateF x y)
pattern Record x = Fix (RecordF x)
pattern Destruct x y = Fix (DestructF x y)
pattern ExprList x = Fix (ExprListF x)
pattern Function x y z = Fix (FunctionF x y z)
pattern TypeDecl x y = Fix (TypeDeclF x y)
pattern ClassFn x y = Fix (ClassFnF x y)
pattern InstanceFn x y z = Fix (InstanceFnF x y z)
pattern Ffi x y = Fix (FfiF x y)
pattern FfiType x y = Fix (FfiTypeF x y)
pattern ExprType x  = Fix (ExprTypeF x)
pattern GetInstance x y = Fix (GetInstanceF x y)
pattern Array x = Fix (ArrayF x)
pattern WithType x y = Fix (WithTypeF x y)
pattern Stmt x = Fix (StmtF x)
pattern If x y z = Fix (IfF x y z)
pattern MD x y = Fix (MDF x y)
pattern In x y = Fix (InF x y)
pattern SetStmt x y = Fix (SetStmtF x y)


pattern ExprListMD x <- MD y (ExprList x)
pattern LabelAccessCall label e = Call (LabelAccess label) e

