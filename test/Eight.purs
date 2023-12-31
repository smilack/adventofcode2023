module Test.AdventOfCode.Twenty23.Eight
  ( main
  ) where

import AdventOfCode.Twenty23.Eight (Direction(..), mkPath, parseInput, parsePath, solve1, solve2)
import Prelude

import Data.Either (Either(..), isRight)
import Data.List.Lazy (nil, (:))
import Data.Map (isEmpty)
import Effect (Effect)
import Effect.Aff (launchAff_)
import JS.BigInt (fromInt)
import Parsing (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Eight" do
    describe "Part 1" do
      let
        path1 = mkPath (R : L : nil)
        path2 = mkPath (L : L : R : nil)
      describe "Makes paths" do
        it "parsePath succeeds" do
          runParser example1 parsePath `shouldSatisfy` isRight
          runParser example2 parsePath `shouldSatisfy` isRight
        it "shows paths" do
          show path1 `shouldEqual` "RLRLRLRLRL..."
          show path2 `shouldEqual` "LLRLLRLLRL..."
      it "finds some nodes" do
        case runParser example1 parseInput of
          Left _ -> fail "example1 parse failed"
          Right { nodes } -> nodes `shouldNotSatisfy` isEmpty
        case runParser example2 parseInput of
          Left _ -> fail "example2 parse failed"
          Right { nodes } -> nodes `shouldNotSatisfy` isEmpty
      it "solve example1" do
        solve1 example1 `shouldEqual` Right 2
      it "solve example2" do
        solve1 example2 `shouldEqual` Right 6
    describe "Part 2" do
      it "solve part 2" do
        solve2 example3 `shouldEqual` Right (fromInt 6)

example1 :: String
example1 =
  """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""

example2 :: String
example2 =
  """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"""

example3 :: String
example3 =
  """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""