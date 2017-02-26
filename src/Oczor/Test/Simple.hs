module Oczor.Test.Simple where

import Test.Hspec
import Oczor.Syntax.Syntax
import ClassyPrelude
import Oczor.Infer.Substitutable

o = hspec $
   describe "lit" $ do
     it "constraints" $ do
      collectConstrainFromTypeExpr (TypeConstraints (singleton ("a", "show")) (TypeVar "a")) `shouldBe` singleton ("a", "show")
      collectConstrainFromTypeExpr (TypeConstraints (singleton ("a", "show")) (TypeConstraints (singleton ("b", "eq")) (TypeVar "b"))) `shouldBe` [("b","eq"),("a","show")]
     it "composeSubst substs" $
      composeSubstList [Subst (mapFromList [("c",TypeVar "e"),("d",TypeVar "e"),("f",TypeVar "e")]),Subst (mapFromList [("e",TypeIdent "Int"),("g",TypeIdent "Int")])] `shouldBe` Subst (mapFromList [("c",TypeIdent "Int"),("d",TypeIdent "Int"),("e",TypeIdent "Int"),("f",TypeIdent "Int"),("g",TypeIdent "Int")])
