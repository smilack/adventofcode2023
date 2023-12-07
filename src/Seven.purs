module AdventOfCode.Twenty23.Seven
  ( cardToHexDigit
  , main
  , parseHand
  , parseInput1
  , solve1
  , typeToHexDigit
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Control.Plus (empty)
import Data.Array (cons, foldr, fromFoldable, reverse, sort)
import Data.Array.NonEmpty (NonEmptyArray, foldMap1, sortWith)
import Data.Either (Either)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (fromStringAs, hexadecimal)
import Data.Map (Map, insertWith, values)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError(..), Parser, fail, runParser)
import Parsing.Combinators (replicateA)
import Parsing.Combinators.Array (many1)
import Parsing.String.Basic (intDecimal, oneOf, skipSpaces)
import PointFree ((<..))

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/7"
  liftEffect do
    log "Part 1:"
    log "part 1 winnings"
    logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

type Hand = { value :: Int, bid :: Int }

typeToHexDigit :: Array Int -> Maybe Char
typeToHexDigit = case _ of
  [ 5 ] -> Just '7'
  [ 4, 1 ] -> Just '6'
  [ 3, 2 ] -> Just '5'
  [ 3, 1, 1 ] -> Just '4'
  [ 2, 2, 1 ] -> Just '3'
  [ 2, 1, 1, 1 ] -> Just '2'
  [ 1, 1, 1, 1, 1 ] -> Just '1'
  _ -> Nothing

cardToHexDigit :: Char -> Maybe Char
cardToHexDigit = case _ of
  'A' -> Just 'E'
  'K' -> Just 'D'
  'Q' -> Just 'C'
  'J' -> Just 'B'
  'T' -> Just 'A'
  '9' -> Just '9'
  '8' -> Just '8'
  '7' -> Just '7'
  '6' -> Just '6'
  '5' -> Just '5'
  '4' -> Just '4'
  '3' -> Just '3'
  '2' -> Just '2'
  _ -> Nothing

solve1 :: String -> Either ParseError Int
solve1 input = do
  hands <- runParser input parseInput1
  let
    sorted = sortWith _.value hands
  pure
    $ unwrap
    $ foldMapWithIndex
        (Additive <.. score)
        sorted
  where
  score i { bid } = (i + 1) * bid

parseInput1 :: Parser String (NonEmptyArray Hand)
parseInput1 = many1 parseHand

parseHand :: Parser String Hand
parseHand = do
  skipSpaces
  cards <- replicateA 5 $ oneOf cardChars
  skipSpaces
  bid <- intDecimal
  let
    hand = foldr insert empty cards
    counts = reverse $ sort $ fromFoldable $ values hand
    digits =
      cons (typeToHexDigit counts)
        (map cardToHexDigit cards)
    digitString =
      fromCharArray <$> sequence digits
  case join (fromStringAs hexadecimal <$> digitString) of
    Nothing -> fail "error making hand"
    Just value -> pure { value, bid }

  where
  insert card = insertWith (+) card 1

cardChars :: Array Char
cardChars = [ 'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2' ]