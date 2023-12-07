module Test.AdventOfCode.Twenty23.Five
  ( main
  ) where

import Prelude

import AdventOfCode.Twenty23.Five (Id(..), Light, Seed, Soil, Temperature, Water, contains, mapName, mapParser, mkMap, mkOneMapping, parseMaps, seedParser, seedParser2, seedRangeParser, solve1, solve2, validSeed)
import AdventOfCode.Twenty23.Util (testParser)
import Data.Array.NonEmpty (cons', prependArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Parsing (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Five" do
    describe "Part 1" do
      it "shows ids" do
        show (Id @Seed 10.0) `shouldEqual` "seed 10.0"
        show (Id @Temperature 10239342.0) `shouldEqual` "temperature 10239342.0"
        show (Id @Seed 4043382508.0) `shouldEqual` "seed 4043382508.0"
      it "parses seeds" do
        testParser input (map Id $ cons' 79.0 [ 14.0, 55.0, 13.0 ]) seedParser
      it "generates map names" do
        mapName @Water @Light `shouldEqual` "water-to-light map:"
      it "maps numbers" do
        (mkOneMapping 50.0 98.0 2.0) 99.0 `shouldEqual` Just 51.0
        (mkOneMapping 50.0 98.0 2.0) 100.0 `shouldEqual` Nothing
        (mkOneMapping 52.0 50.0 48.0) 79.0 `shouldEqual` Just 81.0
      it "makes maps" do
        mkMap @Seed @Soil
          (cons' (mkOneMapping 50.0 98.0 2.0) [ (mkOneMapping 52.0 50.0 48.0) ])
          (Id 79.0)
          `shouldEqual`
            (Id 81.0)
        case runParser "seed-to-soil map:\n50 98 2\n52 50 48" (mapParser @Seed @Soil) of
          Left er -> fail $ show er
          Right seedSoilMap -> seedSoilMap (Id 79.0) `shouldEqual` (Id 81.0)
      it "maps seeds to locations" do
        case runParser input parseMaps of
          Left er -> fail $ show er
          Right almanac -> do
            almanac (Id 79.0) `shouldEqual` Id 82.0
            almanac (Id 14.0) `shouldEqual` Id 43.0
            almanac (Id 55.0) `shouldEqual` Id 86.0
            almanac (Id 13.0) `shouldEqual` Id 35.0
      it "solves part 1" do
        solve1 input `shouldEqual` (Right 35.0)
    describe "Part 2" do
      it "parses seeds" do
        testParser
          input
          ( map Id
              $ prependArray
                  [ 79.0, 80.0, 81.0, 82.0, 83.0, 84.0, 85.0, 86.0, 87.0, 88.0, 89.0, 90.0, 91.0, 92.0 ]
              $ cons' 55.0
                  [ 56.0, 57.0, 58.0, 59.0, 60.0, 61.0, 62.0, 63.0, 64.0, 65.0, 66.0, 67.0 ]
          )
          seedParser2
      it "parses seed ranges" do
        let
          neasr = cons' { start: 79.0, len: 14.0 }
            [ { start: 55.0, len: 13.0 } ]
        testParser input neasr seedRangeParser
      describe "checks if seeds exist" do
        it "0.0 in { start: 0.0, len: 10.0 }" do
          contains 0.0 { start: 0.0, len: 10.0 } `shouldEqual` true
        it "9.0 in { start: 0.0, len: 10.0 }" do
          contains 9.0 { start: 0.0, len: 10.0 } `shouldEqual` true
        it "10.0 in { start: 0.0, len: 10.0 }" do
          contains 10.0 { start: 0.0, len: 10.0 } `shouldEqual` false
        let
          neasr = cons' { start: 11.0, len: 9.0 }
            [ { start: 21.0, len: 9.0 }
            , { start: 0.0, len: 10.0 }
            ]
        it "0.0 valid" do
          validSeed (Id 0.0) neasr `shouldEqual` true
        it "9.0 valid" do
          validSeed (Id 9.0) neasr `shouldEqual` true
        it "10.0 valid" do
          validSeed (Id 10.0) neasr `shouldEqual` false
      it "solves part 2" do
        solve2 input `shouldEqual` (Right 46.0)

input :: String
input =
  """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""