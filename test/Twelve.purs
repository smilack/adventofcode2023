module Test.AdventOfCode.Twenty23.Twelve
  ( main
  ) where

import AdventOfCode.Twenty23.Twelve
import AdventOfCode.Twenty23.Util
import Prelude

import Data.List (List(..), all, any, (:))
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
  describe "Day Twelve" do
    describe "Part 1" do
      describe "parse input" do
        it "example" do
          testParser example exampleParsed parseRecords
        it "validExamples" do
          testParser validExamples validParsed parseRecords
        it "badExamples" do
          testParser badExamples badParsed parseRecords
      describe "validate records" do
        it "validExamples" do
          all validate validParsed `shouldEqual` true
        it "badExamples" do
          any validate badParsed `shouldEqual` false
      pending "other stuff"
    describe "Part 2" do
      pending "more stuff"

example :: String
example =
  """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""

exampleParsed :: List Rec
exampleParsed =
  { rec: "???.###", groups: 1 : 1 : 3 : Nil }
    : { rec: ".??..??...?##.", groups: 1 : 1 : 3 : Nil }
    : { rec: "?#?#?#?#?#?#?#?", groups: 1 : 3 : 1 : 6 : Nil }
    : { rec: "????.#...#...", groups: 4 : 1 : 1 : Nil }
    : { rec: "????.######..#####.", groups: 1 : 6 : 5 : Nil }
    : { rec: "?###????????", groups: 3 : 2 : 1 : Nil }
    : Nil

validExamples :: String
validExamples =
  """#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1
.###.##.#... 3,2,1
.###.##..#.. 3,2,1
.###.##...#. 3,2,1
.###..##.#.. 3,2,1
.###..##..#. 3,2,1
.###..##...# 3,2,1
.###...##.#. 3,2,1
.###...##..# 3,2,1
.###....##.# 3,2,1
#.#.#..#.########### 1,1,1,1,11"""

validParsed :: List Rec
validParsed =
  { rec: "#.#.###", groups: 1 : 1 : 3 : Nil }
    : { rec: ".#...#....###.", groups: 1 : 1 : 3 : Nil }
    : { rec: ".#.###.#.######", groups: 1 : 3 : 1 : 6 : Nil }
    : { rec: "####.#...#...", groups: 4 : 1 : 1 : Nil }
    : { rec: "#....######..#####.", groups: 1 : 6 : 5 : Nil }
    : { rec: ".###.##....#", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###.##.#...", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###.##..#..", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###.##...#.", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###..##.#..", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###..##..#.", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###..##...#", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###...##.#.", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###...##..#", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###....##.#", groups: 3 : 2 : 1 : Nil }
    : { rec: "#.#.#..#.###########", groups: 1 : 1 : 1 : 1 : 11 : Nil }
    : Nil

badExamples :: String
badExamples =
  """##..### 1,1,3
.##.### 1,1,3
.##..##...###. 1,1,3
..........###. 1,1,3
.....##...###. 1,1,3
####.......... 3,2,1
.####........# 3,2,1
.###.......### 3,2,1
"""

badParsed :: List Rec
badParsed =
  { rec: "##..###", groups: 1 : 1 : 3 : Nil }
    : { rec: ".##.###", groups: 1 : 1 : 3 : Nil }
    : { rec: ".##..##...###.", groups: 1 : 1 : 3 : Nil }
    : { rec: "..........###.", groups: 1 : 1 : 3 : Nil }
    : { rec: ".....##...###.", groups: 1 : 1 : 3 : Nil }
    : { rec: "####..........", groups: 3 : 2 : 1 : Nil }
    : { rec: ".####........#", groups: 3 : 2 : 1 : Nil }
    : { rec: ".###.......###", groups: 3 : 2 : 1 : Nil }
    : Nil
