module Test.AdventOfCode.Twenty23.Ten
  ( main
  ) where

import AdventOfCode.Twenty23.Ten
import AdventOfCode.Twenty23.Util
import Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Parsing (runParser)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck ((===), Result)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Ten" do
    describe "Part 1" do
      describe "general" do
        it "move" do
          quickCheck \c d -> c == move (move c d) (opposite d)
        describe "pointsBackTo" do
          describe "Grid 1" do
            it "(1,1) -> Up ==> false" do
              pointsBackTo { x: 1, y: 1 } grid1 Up `shouldEqual` false
            it "(1,1) -> Dn ==> true" do
              pointsBackTo { x: 1, y: 1 } grid1 Dn `shouldEqual` true
            it "(1,1) -> Rt ==> true" do
              pointsBackTo { x: 1, y: 1 } grid1 Rt `shouldEqual` true
            it "(1,1) -> Lf ==> false" do
              pointsBackTo { x: 1, y: 1 } grid1 Lf `shouldEqual` false
          describe "Grid 2" do
            it "(0,2) -> Up ==> false" do
              pointsBackTo { x: 0, y: 2 } grid2 Up `shouldEqual` false
            it "(0,2) -> Dn ==> true" do
              pointsBackTo { x: 0, y: 2 } grid2 Dn `shouldEqual` true
            it "(0,2) -> Rt ==> true" do
              pointsBackTo { x: 0, y: 2 } grid2 Rt `shouldEqual` true
            it "(0,2) -> Lf ==> false" do
              pointsBackTo { x: 0, y: 2 } grid2 Lf `shouldEqual` false
      describe ("example 1" <> show grid1) do
        it "parse input" do
          runParser ex1 parseGrid `shouldEqual` Right grid1
        it "find start location" do
          startLocation grid1 `shouldEqual` Just { x: 1, y: 1 }
        it "valid moves from start" do
          validMoves grid1 S `shouldEqual` Just [ Dn, Rt ]
        it "count steps" do
          countStepsInLoop grid1 `shouldEqual` 8
        it "solve part 1" do
          solve1 ex1 `shouldEqual` Right 4
      describe ("example 2" <> show grid2) do
        it "parse input" do
          runParser ex2 parseGrid `shouldEqual` Right grid2
        it "find start location" do
          startLocation grid2 `shouldEqual` Just { x: 0, y: 2 }
        it "valid moves from start" do
          validMoves grid2 S `shouldEqual` Just [ Dn, Rt ]
        it "count steps" do
          countStepsInLoop grid2 `shouldEqual` 16
        it "solve part 1" do
          solve1 ex2 `shouldEqual` Right 8
    describe "Part 2" do
      pending "more stuff"

ex1 :: String
ex1 =
  """
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
"""

grid1 :: Grid
grid1 = mkGrid
  [ [ Ⲻ, L, 𝖨, Г, 𐐑 ]
  , [ 𐐑, S, Ⲻ, 𐐑, 𝖨 ]
  , [ L, 𝖨, 𐐑, 𝖨, 𝖨 ]
  , [ Ⲻ, L, Ⲻ, 𐐇, 𝖨 ]
  , [ L, 𝖨, Ⲻ, 𐐇, Г ]
  ]

ex2 :: String
ex2 =
  """
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
"""

grid2 :: Grid
grid2 = mkGrid
  [ [ 𐐑, Ⲻ, Г, 𐐑, Ⲻ ]
  , [ O, Г, 𐐇, 𝖨, 𐐑 ]
  , [ S, 𐐇, L, L, 𐐑 ]
  , [ 𝖨, Г, Ⲻ, Ⲻ, 𐐇 ]
  , [ L, 𐐇, O, L, 𐐇 ]
  ]

mkGrid :: Array (Array Pipe) -> Grid
mkGrid = Grid <<< unsNEA <<< map unsNEA
  where
  unsNEA :: forall a. Array a -> NonEmptyArray a
  unsNEA = unsafePartial fromJust <<< fromArray