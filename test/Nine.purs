module Test.AdventOfCode.Twenty23.Nine
  ( main
  ) where

import AdventOfCode.Twenty23.Nine
import AdventOfCode.Twenty23.Util
import Prelude

import Data.List (List(..), fromFoldable, (:))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((===), Result)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Nine" do
    describe "Part 1" do
      it "parse input" do
        testParser example1 parsedExample parseHistories
      it "finds derivatives" do
        derivative ex1a `shouldEqual` ex1a'
        derivative ex2a `shouldEqual` ex2a'
        derivative ex3a `shouldEqual` ex3a'
      pending "other stuff"
    describe "Part 2" do
      pending "more stuff"

example1 :: String
example1 =
  """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""

ex1a :: List Int
ex1a = (0 : 3 : 6 : 9 : 12 : 15 : Nil)

ex1a' :: List Int
ex1a' = (3 : 3 : 3 : 3 : 3 : Nil)

ex2a :: List Int
ex2a = (1 : 3 : 6 : 10 : 15 : 21 : Nil)

ex2a' = (2 : 3 : 4 : 5 : 6 : Nil)

ex3a :: List Int
ex3a = (10 : 13 : 16 : 21 : 30 : 45 : Nil)

ex3a' = (3 : 3 : 5 : 9 : 15 : Nil)

parsedExample :: List (List Int)
parsedExample = ex1a : ex2a : ex3a : Nil