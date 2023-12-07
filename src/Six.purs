module AdventOfCode.Twenty23.Six
  ( NEAR
  , Race
  , findWins
  , main
  , numWins
  , parseRace
  , parseRaces
  , score
  , smartNumWins
  , solve1
  , solve2
  ) where

import AdventOfCode.Twenty23.Util (hSqrt, range', skip)
import Prelude

import Data.Array (length)
import Data.Array.NonEmpty (NonEmptyArray, filter, foldl1, toArray, zipWith)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import JS.BigInt (BigInt, fromString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError, Parser, fail, runParser)
import Parsing.Combinators.Array (many1)
import Parsing.String.Basic (digit, intDecimal, skipSpaces)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/6"
  liftEffect do
    log "Part 1:"
    log "Number of different ways to win"
    logShow $ solve1 input
    log "Part2:"
    log "Number of different ways to win"
    logShow $ solve2 input

type Race a = { time :: a, dist :: a }

type NEAR a = NonEmptyArray (Race a)

parseRaces :: Parser String (NEAR Int)
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

score :: forall a. Ring a => a -> a -> a
score time hold = hold * (time - hold)

findWins :: forall a. Ring a => Ord a => Race a -> Array a
findWins { time, dist } =
  filter ((_ > dist) <<< score time) (range' one $ time - one - one)

numWins :: forall a. Ring a => Ord a => Race a -> Int
numWins = length <<< findWins

solve1 :: String -> Either ParseError Int
solve1 input = do
  races <- runParser input parseRaces
  pure $ foldl1 (*) $ map numWins races

solve2 :: String -> Either ParseError BigInt
solve2 input = map smartNumWins $ runParser input parseRace

parseRace :: Parser String (Race BigInt)
parseRace = do
  skip "Time:"
  time <- intIgnoreSpaces
  skipSpaces
  skip "Distance:"
  dist <- intIgnoreSpaces
  pure $ { time, dist }
  where
  intIgnoreSpaces = do
    digits <- many1 do
      skipSpaces
      digit
    case fromString $ fromCharArray $ toArray digits of
      Nothing -> fail "Couldn't make number from digits"
      Just n -> pure n

-- dist < hold * (time - hold)
-- hold² - time * hold + dist < 0
-- hold < (time ± √(time² - 4 * dist)) / 2
smartNumWins :: forall a. EuclideanRing a => Ord a => Race a -> a
smartNumWins { time, dist } = h1 - h2 + mod
  where
  mod
    | win h1 && win h2 = one
    | lose h1 && lose h2 = -one
    | otherwise = zero
  h1 = (time + discriminant) / two
  h2 = (time - discriminant) / two
  discriminant = hSqrt (time * time - four * dist)
  two = one + (one :: a)
  four = two + two
  win h = dist < score time h
  lose h = dist >= score time h