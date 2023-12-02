module Test.AdventOfCode.Twenty23.One
  ( main
  ) where

import AdventOfCode.Twenty23.One
import AdventOfCode.Twenty23.Util
import Prelude

import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Test.QuickCheck ((===), Result)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (logWriter)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  log "~~~"
  log "Test input:"
  log "~~~"
  log input
  log "~~~"
  launchAff_ $ runSpec [ consoleReporter ] do
    describe "Day One" do
      describe "Part 1" do
        it "Extracts numbers from input strings" do
          parseInput1 input `shouldEqual` inputSplitOnlyNumbers
        it "Combines first and last digit in array" do
          map getNumber (parseInput1 input) `shouldEqual` [ 12, 38, 15, 77 ]
      describe "Part 2" do
        it "Extracts numbers, including spelled out, from input" do
          (map parseInput2 $ lines input2) `shouldEqual` input2SplitNumbers
        it "Combines digits" do
          map getNumber (map parseInput2 $ lines input2) `shouldEqual` input2Numbers
        it "Solves part 2" do
          solve2 input2 `shouldEqual` 281

-- "On each line, the calibration value can be found by combining the first
-- digit and the last digit (in that order) to form a single two-digit number."
-- 1abc2        --> 12
-- pqr3stu8vwx  --> 38
-- a1b2c3d4e5f  --> 15
-- treb7uchet   --> 77
--                  --
--                 142

input :: String
input =
  """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

inputSplitOnlyNumbers :: Array (Array Char)
inputSplitOnlyNumbers =
  [ [ '1', '2' ]
  , [ '3', '8' ]
  , [ '1', '2', '3', '4', '5' ]
  , [ '7' ]
  ]

input2 :: String
input2 =
  """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""

input2SplitNumbers :: Array (Array Char)
input2SplitNumbers =
  [ [ '2', '1', '9' ]
  , [ '8', '2', '3' ]
  , [ '1', '2', '3' ]
  , [ '2', '1', '3', '4' ]
  , [ '4', '9', '8', '7', '2' ]
  , [ '1', '8', '2', '3', '4' ]
  , [ '7', '6' ]
  ]

input2Numbers :: Array Int
input2Numbers = [ 29, 83, 13, 24, 42, 14, 76 ]