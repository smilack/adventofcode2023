module Test.AdventOfCode.Twenty23.Four
  ( main
  ) where

import AdventOfCode.Twenty23.Four (Card, Card2, parseCard, parseCard2, parseCard2s, parseCards, points, process, solve1, solve2, value)
import AdventOfCode.Twenty23.Util (testParser)
import Prelude

import Data.Array (intersect, length, range)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Four" do
    describe "Part 1" do
      it "parseCard" do
        testParser
          "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
          { have: [ 41, 48, 83, 86, 17 ]
          , winners: [ 83, 86, 6, 31, 17, 9, 48, 53 ]
          }
          parseCard
      it "parseCards" do
        testParser input1 parsed1 parseCards
      it "points" do
        points [ 48, 83, 17, 86 ] `shouldEqual` 8
        points [ 32, 61 ] `shouldEqual` 2
        points [ 1, 21 ] `shouldEqual` 2
        points [ 84 ] `shouldEqual` 1
        points [] `shouldEqual` 0
      it "value" do
        value
          { have: [ 41, 48, 83, 86, 17 ]
          , winners: [ 83, 86, 6, 31, 17, 9, 48, 53 ]
          }
          `shouldEqual`
            8
      it "solve1" do
        solve1 input1 `shouldEqual` 13
    describe "Part 2" do
      it "parseCard2" do
        testParser
          "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
          card1
          parseCard2
      it "parseCard2s" do
        testParser input1 parsed2 parseCard2s
      it "process bits" do
        let
          id = 0
          { have, winners } = card1
          pts = length $ intersect have winners
          nextId = id + 1
          ids = range nextId (id + pts)
        ids `shouldEqual` [ 1, 2, 3, 4 ]
      it "process" do
        process parsed2 `shouldEqual` processed2
      it "solve2" do
        solve2 input1 `shouldEqual` solution2

input1 :: String
input1 =
  """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""

parsed1 :: Array Card
parsed1 =
  [ { have: [ 41, 48, 83, 86, 17 ], winners: [ 83, 86, 6, 31, 17, 9, 48, 53 ] }
  , { have: [ 13, 32, 20, 16, 61 ], winners: [ 61, 30, 68, 82, 17, 32, 24, 19 ] }
  , { have: [ 1, 21, 53, 59, 44 ], winners: [ 69, 82, 63, 72, 16, 21, 14, 1 ] }
  , { have: [ 41, 92, 73, 84, 69 ], winners: [ 59, 84, 76, 51, 58, 5, 54, 83 ] }
  , { have: [ 87, 83, 26, 28, 32 ], winners: [ 88, 30, 70, 12, 93, 22, 82, 36 ] }
  , { have: [ 31, 18, 13, 56, 72 ], winners: [ 74, 77, 10, 23, 35, 67, 36, 11 ] }
  ]

parsed2 :: Array Card2
parsed2 =
  [ { id: 1, have: [ 41, 48, 83, 86, 17 ], winners: [ 83, 86, 6, 31, 17, 9, 48, 53 ], copies: 1 }
  , { id: 2, have: [ 13, 32, 20, 16, 61 ], winners: [ 61, 30, 68, 82, 17, 32, 24, 19 ], copies: 1 }
  , { id: 3, have: [ 1, 21, 53, 59, 44 ], winners: [ 69, 82, 63, 72, 16, 21, 14, 1 ], copies: 1 }
  , { id: 4, have: [ 41, 92, 73, 84, 69 ], winners: [ 59, 84, 76, 51, 58, 5, 54, 83 ], copies: 1 }
  , { id: 5, have: [ 87, 83, 26, 28, 32 ], winners: [ 88, 30, 70, 12, 93, 22, 82, 36 ], copies: 1 }
  , { id: 6, have: [ 31, 18, 13, 56, 72 ], winners: [ 74, 77, 10, 23, 35, 67, 36, 11 ], copies: 1 }
  ]

card1 :: Card2
card1 =
  { id: 1
  , have: [ 41, 48, 83, 86, 17 ]
  , winners: [ 83, 86, 6, 31, 17, 9, 48, 53 ]
  , copies: 1
  }

processed2 :: Array Card2
processed2 =
  [ { id: 1, have: [ 41, 48, 83, 86, 17 ], winners: [ 83, 86, 6, 31, 17, 9, 48, 53 ], copies: 1 }
  , { id: 2, have: [ 13, 32, 20, 16, 61 ], winners: [ 61, 30, 68, 82, 17, 32, 24, 19 ], copies: 2 }
  , { id: 3, have: [ 1, 21, 53, 59, 44 ], winners: [ 69, 82, 63, 72, 16, 21, 14, 1 ], copies: 4 }
  , { id: 4, have: [ 41, 92, 73, 84, 69 ], winners: [ 59, 84, 76, 51, 58, 5, 54, 83 ], copies: 8 }
  , { id: 5, have: [ 87, 83, 26, 28, 32 ], winners: [ 88, 30, 70, 12, 93, 22, 82, 36 ], copies: 14 }
  , { id: 6, have: [ 31, 18, 13, 56, 72 ], winners: [ 74, 77, 10, 23, 35, 67, 36, 11 ], copies: 1 }
  ]

solution2 :: Int
solution2 = 30

