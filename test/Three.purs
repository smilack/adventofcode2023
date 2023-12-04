module Test.AdventOfCode.Twenty23.Three
  ( input1
  , main
  ) where

import AdventOfCode.Twenty23.Three (Location, gearRatio, includes, locateStars, region, solve1, solve2, symbolInRegion)
import AdventOfCode.Twenty23.Util (testParser, to2dArray)
import Prelude

import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
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
      it "Calculates gear ratio" do
        gearRatio [ { number: 467 }, { number: 35 }, { number: 617 } ] `shouldEqual` 0
        gearRatio [ { number: 467 }, { number: 35 } ] `shouldEqual` 16345
        gearRatio [ { number: 617 } ] `shouldEqual` 0
        gearRatio [] `shouldEqual` 0
      it "Determines if Point is in Location" do
        includes { line: 1, column: 3 } loc1 `shouldEqual` true
        includes { line: 1, column: 3 } loc2 `shouldEqual` false
      it "Finds stars" do
        testParser input1 [ { line: 1, column: 3 }, { line: 4, column: 3 }, { line: 8, column: 5 } ] locateStars
      it "Solves part 2" do
        solve2 input1 `shouldEqual` 467835

input1 :: String
input1 =
  """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
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
