module Test.AdventOfCode.Twenty23.Eleven
  ( example
  , main
  ) where

import AdventOfCode.Twenty23.Eleven (Coord, _col, _row, expand, findEmpty, findGalaxies, getShortestPaths, isEmpty, parseImage, solve1, solve2)
import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import JS.BigInt (fromInt)
import Parsing (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Eleven" do
    describe "Part 1" do
      case runParser example parseImage of
        Left _ ->
          it "parse input" $ fail "parse error"
        Right i -> do
          it "parse input" $ show i `shouldEqual` example
          describe "isEmpty" do
            it "_col 0 false" $ isEmpty (_col 0) i `shouldEqual` false
            it "_col 1 false" $ isEmpty (_col 1) i `shouldEqual` false
            it "_col 2 true" $ isEmpty (_col 2) i `shouldEqual` true
            it "_col 3 false" $ isEmpty (_col 3) i `shouldEqual` false
            it "_row 0 false" $ isEmpty (_row 0) i `shouldEqual` false
            it "_row 1 false" $ isEmpty (_row 1) i `shouldEqual` false
            it "_row 2 false" $ isEmpty (_row 2) i `shouldEqual` false
            it "_row 3 true" $ isEmpty (_row 3) i `shouldEqual` true
          it "empty rows" do
            findEmpty _row i `shouldEqual` [ 3, 7 ]
          it "empty cols" do
            findEmpty _col i `shouldEqual` [ 2, 5, 8 ]
          let
            ex = expand i
          it "expand" do
            show ex `shouldEqual` expanded
          let
            gals = findGalaxies ex
          it "findGalaxies" do
            gals `shouldEqual` galaxies
          it "shortest paths" do
            getShortestPaths gals `shouldEqual` 374
      it "solve part1" do
        solve1 example `shouldEqual` Right 374
    describe "Part 2" do
      it "solve part2" do
        solve2 example `shouldEqual` Right (fromInt 82000210)

example :: String
example =
  """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""

expanded :: String
expanded =
  """....#........
.........#...
#............
.............
.............
........#....
.#...........
............#
.............
.............
.........#...
#....#......."""

galaxies :: Array Coord
galaxies =
  [ { x: 4, y: 0 }
  , { x: 9, y: 1 }
  , { x: 0, y: 2 }
  , { x: 8, y: 5 }
  , { x: 1, y: 6 }
  , { x: 12, y: 7 }
  , { x: 9, y: 10 }
  , { x: 0, y: 11 }
  , { x: 5, y: 11 }
  ]

--  0123456789ABC
-- 0....1........
-- 1.........2...
-- 23............
-- 3.............
-- 4.............
-- 5........4....
-- 6.5...........
-- 7............6
-- 8.............
-- 9.............
-- A.........7...
-- B8....9.......
