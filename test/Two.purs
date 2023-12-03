module Test.AdventOfCode.Twenty23.Two
  ( input1
  , main
  ) where

import AdventOfCode.Twenty23.Two (Handful(..), bag, isPossible, parseColor, parseGame, power, toGame)
import AdventOfCode.Twenty23.Util (testParser)
import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Two" do
    describe "Part 1" do
      it "parseColor reads colors correctly" do
        testParser " 3 blue" hfs.b3 parseColor
        testParser " 4 red" hfs.r4 parseColor
        testParser "2 green" hfs.g2 parseColor
      it "Handful is a Monoid" do
        (hfs.r4 <> hfs.g2 <> hfs.b3) `shouldEqual` Handful { r: 4, g: 2, b: 3 }
        ((hfs.r4 <> hfs.g2) <> hfs.b3) `shouldEqual` (hfs.r4 <> (hfs.g2 <> hfs.b3))
        (hfs.r4 <> mempty) `shouldEqual` hfs.r4
        (mempty <> hfs.r4) `shouldEqual` hfs.r4
      it "toGame converts Handful to Game" do
        toGame 1 hfs.r4 `shouldEqual` { id: 1, r: 4, g: 0, b: 0 }
      it "parseGame parses a line of input" do
        testParser
          "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
          { id: 3, r: 20, g: 13, b: 6 }
          parseGame
      it "isPossible works" do
        isPossible bag { id: 4, r: 14, g: 3, b: 15 } `shouldEqual` false
        isPossible bag { id: 1, r: 4, g: 2, b: 6 } `shouldEqual` true
    describe "Part 2" do
      it "calculates power of handful and game" do
        (power $ unwrap $ Handful { r: 4, g: 2, b: 3 }) `shouldEqual` 24
        power { id: 1, r: 4, g: 2, b: 6 } `shouldEqual` 48

hfs :: { b3 :: Handful, g2 :: Handful, r4 :: Handful }
hfs =
  { r4: Handful { r: 4, g: 0, b: 0 }
  , g2: Handful { r: 0, g: 2, b: 0 }
  , b3: Handful { r: 0, g: 0, b: 3 }
  }

input1 :: String
input1 =
  """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""