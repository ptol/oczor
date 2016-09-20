module Oczor.Test.TestEngine where

import ClassyPrelude hiding (try)
import Test.Hspec

import Oczor.Test.Files

checkDir (d,f) = do
  testSets <- dirTestSets d
  hspec $ describe d $ traverse_ (checkTestSet f) testSets

checkTestSet :: CheckFunc -> TestSet -> SpecWith ()
checkTestSet f (name, list) = describe name $ traverse_ (checkTest f) list

checkTest :: CheckFunc -> Test -> SpecWith ()
checkTest f (id, (l,r)) = it (show id ++ ": " ++ show l) $
  either expectationFailure (\x -> expectTrue (r ++ "\n-\n" ++ x) (x == r)) (f l)

expectTrue msg b = unless b (expectationFailure msg)

