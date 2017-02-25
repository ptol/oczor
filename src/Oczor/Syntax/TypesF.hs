{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Oczor.Syntax.TypesF where
import ClassyPrelude
import Data.Functor.Foldable hiding (Foldable)
import Data.Functor.Classes

type ClassName = String
type ConstraintSetF e = [(e, ClassName)]

data TypeExprF e =
  TypeIdentF String |
  TypeVarF String |
  TypeUnionF [e] |
  TypePolyF [e] e |
  TypeConstraintsF (ConstraintSetF e) e |
  TypeFuncF e e |
  TypeLabelF String e |
  TypeApplyF e [e] |
  NoTypeF |
  TypeRecordF [e] |
  TypeRowF e [e]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Eq1 TypeExprF where
  liftEq _ (TypeIdentF a) (TypeIdentF b) = a == b
  liftEq _ (TypeVarF a) (TypeVarF b) = a == b
  liftEq f (TypeUnionF a) (TypeUnionF b) = liftEq f a b
  liftEq f (TypePolyF aa a) (TypePolyF bb b) =
    eqCompose (liftEq f aa bb) (f a b)
  liftEq f (TypeConstraintsF aa a) (TypeConstraintsF bb b) =
    eqCompose (liftListEq f aa bb) (f a b)
  liftEq f (TypeFuncF aa a) (TypeFuncF bb b) =
    eqCompose (f aa bb) (f a b)
  liftEq f (TypeLabelF a aa) (TypeLabelF b bb) =
    eqCompose (a == b) (f aa bb)
  liftEq _ NoTypeF NoTypeF = True
  liftEq f (TypeRecordF aa) (TypeRecordF bb) = liftEq f aa bb
  liftEq f (TypeApplyF a aa) (TypeApplyF b bb) =
    eqCompose (f a b) (liftEq f aa bb)
  liftEq f (TypeRowF a aa) (TypeRowF b bb) =
    eqCompose (f a b) (liftEq f aa bb)
  
  liftEq _ a b = if c == EQ then error "UNIMPL 438" else False where
    c = tefIndex a `compare` tefIndex b

instance Ord1 TypeExprF where
  liftCompare _ (TypeIdentF a) (TypeIdentF b) = a `compare` b
  liftCompare _ (TypeVarF a) (TypeVarF b) = a `compare` b
  liftCompare f (TypeUnionF a) (TypeUnionF b) = liftCompare f a b
  liftCompare f (TypePolyF aa a) (TypePolyF bb b) = 
    ordCompose (liftCompare f aa bb) (f a b)
  liftCompare f (TypeConstraintsF aa a) (TypeConstraintsF bb b) =
    ordCompose (liftList f aa bb) (f a b)
  liftCompare f (TypeFuncF aa a) (TypeFuncF bb b) =
    ordCompose (f aa bb) (f a b)
  liftCompare f (TypeLabelF a aa) (TypeLabelF b bb) =
    ordCompose (a `compare` b) (f aa bb)
  liftCompare _ NoTypeF NoTypeF = EQ
  liftCompare f (TypeRecordF aa) (TypeRecordF bb) = liftCompare f aa bb
  liftCompare f (TypeApplyF a aa) (TypeApplyF b bb) =
    ordCompose (f a b) (liftCompare f aa bb)
  liftCompare f (TypeRowF a aa) (TypeRowF b bb) =
    ordCompose (f a b) (liftCompare f aa bb)
  liftCompare _ a b = if c == EQ then error "UNIMPL 090" else c where
    c = tefIndex a `compare` tefIndex b

liftListEq f a b = eqCompose
  (liftEq f (map fst a) (map fst b))
  ((==)     (map snd a) (map snd b))


liftList f a b = ordCompose
  (liftCompare f (map fst a) (map fst b))
  (compare       (map snd a) (map snd b))

instance Read1 TypeExprF where
  liftReadsPrec = error "UNIMPL 903"

ordCompose EQ b = b
ordCompose a _ = a

eqCompose a b = a && b

tefIndex x = case x of
  TypeIdentF _    -> 0

  TypeVarF _      -> 1
  TypeUnionF _    -> 2
  TypePolyF _ _   -> 3
  TypeConstraintsF _ _ -> 4
  TypeFuncF _ _   -> 5
  TypeLabelF _ _  -> 6
  TypeApplyF _ _  -> 7
  NoTypeF         -> 8
  TypeRecordF _   -> 9
  TypeRowF _ _    -> 10

instance Show (Fix TypeExprF) where showsPrec p (Fix x) = showsPrec p x
