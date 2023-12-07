module AdventOfCode.Twenty23.Six
  ( NEAR
  , Race
  , findWins
  , main
  , numWins
  , parseRaces
  , score
  , solve1
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Control.Monad.List.Trans (foldl)
import Data.Array (filter, length, range)
import Data.Array.NonEmpty (NonEmptyArray, foldl1, zipWith)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError(..), Parser, runParser)
import Parsing.Combinators.Array (many1)
import Parsing.String.Basic (intDecimal, skipSpaces)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/6"
  liftEffect do
    log "Part 1:"
    log "Number of different ways to win"
    logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

type Race = { time :: Int, dist :: Int }

type NEAR = NonEmptyArray Race

parseRaces :: Parser String NEAR
parseRaces = do
  skip "Time:"
  times <- intList
  skipSpaces
  skip "Distance:"
  distances <- intList
  pure $ zipWith { time: _, dist: _ } times distances
  where
  intList = many1 do
    skipSpaces
    intDecimal

score :: Int -> Int -> Int
score time hold = hold * (time - hold)

findWins :: Race -> Array Int
findWins { time, dist } =
  filter ((_ > dist) <<< score time) (range 1 $ time - 1)

numWins :: Race -> Int
numWins = length <<< findWins

solve1 :: String -> Either ParseError Int
solve1 input = do
  races <- runParser input parseRaces
  pure $ foldl1 (*) $ map numWins races