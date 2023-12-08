module Test.AdventOfCode.Twenty23.Eight
  ( main
  ) where

import AdventOfCode.Twenty23.Eight
import AdventOfCode.Twenty23.Util
import Prelude

import Data.Either (isLeft)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Parsing (runParser)
import Test.QuickCheck ((===), Result)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (expectError, fail, shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Eight" do
    describe "Part 1" do
      pending "parse input"
      pending "other stuff"
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
ZZZ = (ZZZ, ZZZ)"""

example2 :: String
example2 =
  """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"""