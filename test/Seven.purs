module Test.AdventOfCode.Twenty23.Seven
  ( main
  ) where

import AdventOfCode.Twenty23.Seven
import AdventOfCode.Twenty23.Util
import Prelude

import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Map (insert)
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
  describe "Day Seven" do
    describe "Part 1" do
      -- it "parse hand" do
      --   testParser
      --     "32T3K 765"
      --     { value: 0x232A3D, bid: 765 }
      --     parseHand
      --   testParser
      --     "T55J5 684"
      --     { value: 0x4A55B5, bid: 684 }
      --     parseHand
      it "solve1" do
        solve1 input `shouldEqual` Right 6440
    describe "Part 2" do
      -- it "parses part 2" do
      --   let
      --     hand2 =
      --       { nats: insert 'T' 1 $ insert '5' 3 empty
      --       , wilds: 1
      --       , str: "T55J5"
      --       , bid: 684
      --       }
      --   testParser "T55J5 684" hand2 parseHand2
      --   toHand hand2 `shouldEqual`
      --     { value: 0x6A5515, bid: 684 }
      it "solves part 2" do
        solve2 input `shouldEqual` Right 5905

input :: String
input =
  """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""