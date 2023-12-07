module Test.AdventOfCode.Twenty23.Six
  ( main
  ) where

import AdventOfCode.Twenty23.Six (NEAR, Race, findWins, numWins, parseRace, parseRaces, smartNumWins, solve1, solve2)
import AdventOfCode.Twenty23.Util (testParser)
import Prelude

import Data.Array.NonEmpty (cons')
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import JS.BigInt (BigInt, fromInt)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
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
      it "parses input" do
        testParser input p2race parseRace
      it "numWins but smart" do
        map smartNumWins parsed
          `shouldEqual`
            (cons' 4 [ 8, 9 ])
      it "solve2" do
        solve2 input `shouldEqual` Right (fromInt 71503)

input :: String
input = "Time:      7  15   30\nDistance:  9  40  200"

parsed :: NEAR Int
parsed = cons'
  { time: 7, dist: 9 }
  [ { time: 15, dist: 40 }
  , { time: 30, dist: 200 }
  ]

p2race :: Race BigInt
p2race =
  { time: fromInt 71530
  , dist: fromInt 940200
  }