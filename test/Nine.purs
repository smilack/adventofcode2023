module Test.AdventOfCode.Twenty23.Nine
  ( main
  ) where

import AdventOfCode.Twenty23.Nine (derivative, findConstFn, next, parseHistories, solve1, solve2)
import AdventOfCode.Twenty23.Util (testParser)
import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList, fromList)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
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
      it "finds all derivatives until reaching a constant fn" do
        findConstFn ex1a `shouldEqual` unsFromL (ex1a : ex1a' : ex1a'' : Nil)
      it "predicts next number for list" do
        next (findConstFn ex1a) `shouldEqual` 18
        next (findConstFn ex2a) `shouldEqual` 28
        next (findConstFn ex3a) `shouldEqual` 68
      it "solve part1" do
        solve1 example1 `shouldEqual` (Right 114)
    describe "Part 2" do
      it "solve part2" do
        solve2 example1 `shouldEqual` (Right 2)

example1 :: String
example1 =
  """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""

ex1a :: NonEmptyList Int
ex1a = unsFromL (0 : 3 : 6 : 9 : 12 : 15 : Nil)

ex1a' :: NonEmptyList Int
ex1a' = unsFromL (3 : 3 : 3 : 3 : 3 : Nil)

ex1a'' :: NonEmptyList Int
ex1a'' = unsFromL (0 : 0 : 0 : 0 : Nil)

ex2a :: NonEmptyList Int
ex2a = unsFromL (1 : 3 : 6 : 10 : 15 : 21 : Nil)

ex2a' :: NonEmptyList Int
ex2a' = unsFromL (2 : 3 : 4 : 5 : 6 : Nil)

ex3a :: NonEmptyList Int
ex3a = unsFromL (10 : 13 : 16 : 21 : 30 : 45 : Nil)

ex3a' :: NonEmptyList Int
ex3a' = unsFromL (3 : 3 : 5 : 9 : 15 : Nil)

parsedExample :: NonEmptyList (NonEmptyList Int)
parsedExample = unsFromL (ex1a : ex2a : ex3a : Nil)

unsFromL :: forall a. List a -> NonEmptyList a
unsFromL = unsafePartial fromJust <<< fromList
