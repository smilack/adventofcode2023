module AdventOfCode.Twenty23.Four
  ( Card
  , Card2
  , copy
  , main
  , parseCard
  , parseCard2
  , parseCard2s
  , parseCards
  , points
  , process
  , solve1
  , solve2
  , value
  ) where

import AdventOfCode.Twenty23.Util (modify, sumMap)
import Prelude

import Data.Array (intersect, length, modifyAtIndices, range, (!!))
import Data.Either (Either(..), either)
import Data.Foldable (sum)
import Data.Int (pow)
import Data.Maybe (Maybe(..))
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
    log "does the card stuff"
    logShow $ solve2 input

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

value :: forall r. { have :: Array Int, winners :: Array Int | r } -> Int
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

solve2 :: String -> Int
solve2 input =
  either
    (const 0)
    (sumMap (_.copies) <<< process)
    $ runParser input parseCard2s

process :: Array Card2 -> Array Card2
process = go 0
  where
  go id array =
    case array !! id of
      Nothing -> array
      Just { copies, have, winners } ->
        let
          pts = length $ intersect have winners
          nextId = id + 1
          ids = if pts > 0 then range nextId (id + pts) else []
        in
          go nextId (copy ids copies array)

copy :: Array Int -> Int -> Array Card2 -> Array Card2
copy ids times array = modifyAtIndices ids (modify @"copies" (add times)) array
