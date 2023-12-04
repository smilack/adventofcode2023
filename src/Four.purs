module AdventOfCode.Twenty23.Four
  ( main
  , Card
  , parseCard
  , parseCards
  , value
  , points
  , solve1
  , Card2
  , parseCard2s
  , parseCard2
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Data.Array (intersect, length)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (pow)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators.Array (many)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, skipSpaces)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/4"
  liftEffect do
    log "Part 1:"
    log "Sum of card values"
    logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

type Card = { have :: Array Int, winners :: Array Int }

parseCards :: Parser String (Array Card)
parseCards = many parseCard

parseCard :: Parser String Card
parseCard = do
  skipSpaces
  _ <- string "Card"
  skipSpaces
  _ <- intDecimal
  _ <- string ":"
  have <- many do
    skipSpaces
    intDecimal
  skipSpaces
  _ <- string "|"
  winners <- many do
    skipSpaces
    intDecimal
  pure { have, winners }

value :: Card -> Int
value { have, winners } = points $ intersect have winners

points :: Array Int -> Int
points = length >>> case _ of
  len
    | len <= 0 -> 0
    | otherwise -> pow 2 (len - 1)

solve1 :: String -> Int
solve1 input = case runParser input parseCards of
  Left _ -> 0
  Right cards -> sum $ map value cards

type Card2 = { id :: Int, have :: Array Int, winners :: Array Int, copies :: Int }

parseCard2s :: Parser String (Array Card2)
parseCard2s = many parseCard2

parseCard2 :: Parser String Card2
parseCard2 = do
  skipSpaces
  _ <- string "Card"
  skipSpaces
  id <- intDecimal
  _ <- string ":"
  have <- many do
    skipSpaces
    intDecimal
  skipSpaces
  _ <- string "|"
  winners <- many do
    skipSpaces
    intDecimal
  pure { id, have, winners, copies: 1 }