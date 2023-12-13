module Test.AdventOfCode.Twenty23.Twelve
  ( main
  ) where

import AdventOfCode.Twenty23.Twelve
import AdventOfCode.Twenty23.Util
import Prelude

import Data.Either (Either(..))
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
      it "countPossibilities" do
        map countPossibilities exampleParsed
          `shouldEqual`
            (1 : 4 : 1 : 1 : 4 : 10 : Nil)
      it "solve1" do
        solve1 example `shouldEqual` Right 21
    describe "Part 2" do
      it "unfold record" do
        unfold unfoldEx `shouldEqual` unfoldEx'
      it "solve2" do
        solve2 example `shouldEqual` Right 525152

example :: String
example =
  """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""

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

unfoldEx :: Rec
unfoldEx =
  { springs: Unknown : Unknown : Unknown : Good : Bad : Bad : Bad : Nil, groups: 1 : 1 : 3 : Nil }

unfoldEx' :: Rec
unfoldEx' =
  { springs: Unknown : Unknown : Unknown : Good : Bad : Bad : Bad : Unknown : Unknown : Unknown : Unknown : Good : Bad : Bad : Bad : Unknown : Unknown : Unknown : Unknown : Good : Bad : Bad : Bad : Unknown : Unknown : Unknown : Unknown : Good : Bad : Bad : Bad : Unknown : Unknown : Unknown : Unknown : Good : Bad : Bad : Bad : Nil
  , groups: 1 : 1 : 3 : 1 : 1 : 3 : 1 : 1 : 3 : 1 : 1 : 3 : 1 : 1 : 3 : Nil
  }

exampleParsed :: List Rec
exampleParsed =
  { springs: Unknown : Unknown : Unknown : Good : Bad : Bad : Bad : Nil, groups: 1 : 1 : 3 : Nil }
    : { springs: Good : Unknown : Unknown : Good : Good : Unknown : Unknown : Good : Good : Good : Unknown : Bad : Bad : Good : Nil, groups: 1 : 1 : 3 : Nil }
    : { springs: Unknown : Bad : Unknown : Bad : Unknown : Bad : Unknown : Bad : Unknown : Bad : Unknown : Bad : Unknown : Bad : Unknown : Nil, groups: 1 : 3 : 1 : 6 : Nil }
    : { springs: Unknown : Unknown : Unknown : Unknown : Good : Bad : Good : Good : Good : Bad : Good : Good : Good : Nil, groups: 4 : 1 : 1 : Nil }
    : { springs: Unknown : Unknown : Unknown : Unknown : Good : Bad : Bad : Bad : Bad : Bad : Bad : Good : Good : Bad : Bad : Bad : Bad : Bad : Good : Nil, groups: 1 : 6 : 5 : Nil }
    : { springs: Unknown : Bad : Bad : Bad : Unknown : Unknown : Unknown : Unknown : Unknown : Unknown : Unknown : Unknown : Nil, groups: 3 : 2 : 1 : Nil }
    : Nil

validParsed :: List Rec
validParsed =
  { springs: Bad : Good : Bad : Good : Bad : Bad : Bad : Nil, groups: 1 : 1 : 3 : Nil }
    : { springs: Good : Bad : Good : Good : Good : Bad : Good : Good : Good : Good : Bad : Bad : Bad : Good : Nil, groups: 1 : 1 : 3 : Nil }
    : { springs: Good : Bad : Good : Bad : Bad : Bad : Good : Bad : Good : Bad : Bad : Bad : Bad : Bad : Bad : Nil, groups: 1 : 3 : 1 : 6 : Nil }
    : { springs: Bad : Bad : Bad : Bad : Good : Bad : Good : Good : Good : Bad : Good : Good : Good : Nil, groups: 4 : 1 : 1 : Nil }
    : { springs: Bad : Good : Good : Good : Good : Bad : Bad : Bad : Bad : Bad : Bad : Good : Good : Bad : Bad : Bad : Bad : Bad : Good : Nil, groups: 1 : 6 : 5 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Bad : Bad : Good : Good : Good : Good : Bad : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Bad : Bad : Good : Bad : Good : Good : Good : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Bad : Bad : Good : Good : Bad : Good : Good : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Bad : Bad : Good : Good : Good : Bad : Good : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Good : Bad : Bad : Good : Bad : Good : Good : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Good : Bad : Bad : Good : Good : Bad : Good : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Good : Bad : Bad : Good : Good : Good : Bad : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Good : Good : Bad : Bad : Good : Bad : Good : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Good : Good : Bad : Bad : Good : Good : Bad : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Good : Good : Good : Bad : Bad : Good : Bad : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Bad : Good : Bad : Good : Bad : Good : Good : Bad : Good : Bad : Bad : Bad : Bad : Bad : Bad : Bad : Bad : Bad : Bad : Bad : Nil, groups: 1 : 1 : 1 : 1 : 11 : Nil }
    : Nil

badParsed :: List Rec
badParsed =
  { springs: Bad : Bad : Good : Good : Bad : Bad : Bad : Nil, groups: 1 : 1 : 3 : Nil }
    : { springs: Good : Bad : Bad : Good : Bad : Bad : Bad : Nil, groups: 1 : 1 : 3 : Nil }
    : { springs: Good : Bad : Bad : Good : Good : Bad : Bad : Good : Good : Good : Bad : Bad : Bad : Good : Nil, groups: 1 : 1 : 3 : Nil }
    : { springs: Good : Good : Good : Good : Good : Good : Good : Good : Good : Good : Bad : Bad : Bad : Good : Nil, groups: 1 : 1 : 3 : Nil }
    : { springs: Good : Good : Good : Good : Good : Bad : Bad : Good : Good : Good : Bad : Bad : Bad : Good : Nil, groups: 1 : 1 : 3 : Nil }
    : { springs: Bad : Bad : Bad : Bad : Good : Good : Good : Good : Good : Good : Good : Good : Good : Good : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Bad : Good : Good : Good : Good : Good : Good : Good : Good : Bad : Nil, groups: 3 : 2 : 1 : Nil }
    : { springs: Good : Bad : Bad : Bad : Good : Good : Good : Good : Good : Good : Good : Bad : Bad : Bad : Nil, groups: 3 : 2 : 1 : Nil }
    : Nil
