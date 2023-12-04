module Test.AdventOfCode.Twenty23.Three
  ( input1
  , main
  ) where

import AdventOfCode.Twenty23.Three
import AdventOfCode.Twenty23.Util
import Prelude

import Data.String (split)
import Data.String.CodeUnits (toCharArray)
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
  describe "Day Three" do
    describe "Part 1" do
      it "Finds region around number" do
        region input1As2d loc1 `shouldEqual` reg1
        region input1As2d loc2 `shouldEqual` reg2
      it "Determines if symbol exists in region" do
        symbolInRegion reg1 `shouldEqual` true
        symbolInRegion reg2 `shouldEqual` true
        symbolInRegion reg3 `shouldEqual` false
      it "Finds sum of numbers adjacent to symbols" do
        solve1 input1 `shouldEqual` 4361
    describe "Part 2" do
      pending "more stuff"

input1 :: String
input1 =
  """467..114..
...*......
..35..633.
......#...
617*......
.......58.
.-592.....
......755.
...$.*....
.664.598.."""

input1As2d :: Array (Array Char)
input1As2d = to2dArray input1

loc1 :: Location
loc1 = { line: 0, column: 0, length: 3, number: 467 }

loc2 :: Location
loc2 = { line: 6, column: 2, length: 3, number: 592 }

reg1 :: Array Char
reg1 = toCharArray "467....*"

reg2 :: Array Char
reg2 = toCharArray "....+.592......"

reg3 :: Array Char
reg3 = toCharArray ".....58....."
