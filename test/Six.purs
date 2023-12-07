module Test.AdventOfCode.Twenty23.Six
  ( main
  ) where

import AdventOfCode.Twenty23.Six
import AdventOfCode.Twenty23.Util
import Prelude

import Data.Array.NonEmpty (cons')
import Data.Either (Either(..))
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
  describe "Day Six" do
    describe "Part 1" do
      it "parses input" do
        testParser input parsed parseRaces
      it "findsWins" do
        findWins
          { time: 7, dist: 9 }
          `shouldEqual`
            [ 2, 3, 4, 5 ]
      it "numWins" do
        numWins { time: 15, dist: 40 } `shouldEqual` 8
        numWins { time: 30, dist: 200 } `shouldEqual` 9
      it "solve1" do
        solve1 input `shouldEqual` Right 288
    describe "Part 2" do
      pending "more stuff"

input :: String
input = "Time:      7  15   30\nDistance:  9  40  200"

parsed :: NEAR
parsed = cons'
  { time: 7, dist: 9 }
  [ { time: 15, dist: 40 }
  , { time: 30, dist: 200 }
  ]