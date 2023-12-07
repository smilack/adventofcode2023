module AdventOfCode.Twenty23.Seven
  ( Hand2
  , addCard
  , cardChars
  , cardToHexDigit
  , emptyHand
  , main
  , parseHand
  , parseHand2
  , parseInput1
  , solve1
  , solve2
  , toHand
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
import Data.Lens (Lens', over)
import Data.Lens.Index (ix)
import Data.Map (Map, insertWith, values)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
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
    log "part 2 winnings"
    logShow $ solve2 input

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

type Hand2 =
  { nats :: Map Char Int
  , wilds :: Int
  , str :: String
  , bid :: Int
  }

toHand :: Hand2 -> Hand
toHand { nats, wilds, str, bid } =
  let
    counts =
      case reverse $ sort $ fromFoldable $ values nats of
        [] -> [ 0 ] -- all wilds
        a -> a
    counts' = over (ix 0) (_ + wilds) counts
    digits =
      cons (typeToHexDigit counts')
        (map cardToHexDigit2 $ toCharArray str)
    digitString =
      fromCharArray <$> sequence digits
  in
    case join (fromStringAs hexadecimal <$> digitString) of
      Nothing -> { value: 0, bid: 0 }
      Just value -> { value, bid }

emptyHand :: Hand2
emptyHand = { nats: empty, wilds: 0, str: "", bid: 0 }

addCard :: Char -> Hand2 -> Hand2
addCard card hand@{ nats, wilds, str } =
  case card of
    'A' -> hand'
    'K' -> hand'
    'Q' -> hand'
    'T' -> hand'
    '9' -> hand'
    '8' -> hand'
    '7' -> hand'
    '6' -> hand'
    '5' -> hand'
    '4' -> hand'
    '3' -> hand'
    '2' -> hand'
    'J' -> hand { wilds = wilds + 1, str = str' }
    _ -> hand
  where
  nats' = insertWith (+) card 1 nats
  str' = singleton card <> str
  hand' = hand { nats = nats', str = str' }

parseHand2 :: Parser String Hand2
parseHand2 = do
  skipSpaces
  cards <- replicateA 5 $ oneOf cardChars
  skipSpaces
  bid <- intDecimal
  let
    hand = foldr addCard emptyHand cards
  pure $ hand { bid = bid }

cardToHexDigit2 :: Char -> Maybe Char
cardToHexDigit2 = case _ of
  'A' -> Just 'E'
  'K' -> Just 'D'
  'Q' -> Just 'C'
  'T' -> Just 'A'
  '9' -> Just '9'
  '8' -> Just '8'
  '7' -> Just '7'
  '6' -> Just '6'
  '5' -> Just '5'
  '4' -> Just '4'
  '3' -> Just '3'
  '2' -> Just '2'
  'J' -> Just '1'
  _ -> Nothing

solve2 :: String -> Either ParseError Int
solve2 input = do
  hands <- runParser input $ many1 parseHand2
  let
    sorted = sortWith _.value $ map toHand hands
  pure
    $ unwrap
    $ foldMapWithIndex
        (Additive <.. score)
        sorted
  where
  score i { bid } = (i + 1) * bid