module Test.AdventOfCode.Twenty23.Eight
  ( main
  ) where

import AdventOfCode.Twenty23.Eight
import AdventOfCode.Twenty23.Util
import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), isLeft, isRight)
import Data.Generic.Rep (class Generic)
import Data.List.Lazy (List(..), nil, (:))
import Data.Map (isEmpty)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable1 (class Unfoldable1)
import Data.Unfoldable1 (iterateN) as Unf
import Effect (Effect)
import Effect.Aff (launchAff_)
import Parsing (Parser, runParser)
import Parsing.Combinators ((<?>))
import Parsing.String (char)
import Test.QuickCheck ((===), Result)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (expectError, fail, shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
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
      pending "more stuff"

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