module AdventOfCode.Twenty23.Seven
  ( main
  , solve1
  , solve2
  ) where

import Prelude

import Control.Plus (empty)
import Data.Array (foldr, fromFoldable, reverse, sort)
import Data.Array.NonEmpty (sortWith)
import Data.Either (Either)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (pow)
import Data.Lens (over)
import Data.Lens.Index (ix)
import Data.List (List(..))
import Data.Map (Map, insertWith, pop, values)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError, Parser, fail, runParser)
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

solve1 :: String -> Either ParseError Int
solve1 = solve @JacksNatural

solve2 :: String -> Either ParseError Int
solve2 = solve @JacksWild

solve :: forall @a. JackValue a => String -> Either ParseError Int
solve input = do
  hands <- runParser input $ many1 (parseHand @a)
  pure
    $ unwrap
    $ foldMapWithIndex (Additive <.. score)
    $ sortWith _.value hands
  where
  score i { bid } = (i + 1) * bid

data Card
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two

derive instance Generic Card _
derive instance Eq Card
derive instance Ord Card

cardChars :: Array Char
cardChars = [ 'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2' ]

type Hand :: ValueKind -> Type
type Hand a = { value :: Int, bid :: Int }

parseHand :: forall @a. JackValue a => Parser String (Hand a)
parseHand = do
  skipSpaces
  cards <- replicateA 5 parseCard
  skipSpaces
  bid <- intDecimal
  pure { value: handValue @a cards, bid }

parseCard :: Parser String Card
parseCard = do
  char <- oneOf cardChars
  case char of
    'A' -> pure Ace
    'K' -> pure King
    'Q' -> pure Queen
    'J' -> pure Jack
    'T' -> pure Ten
    '9' -> pure Nine
    '8' -> pure Eight
    '7' -> pure Seven
    '6' -> pure Six
    '5' -> pure Five
    '4' -> pure Four
    '3' -> pure Three
    '2' -> pure Two
    x -> fail $ "oneOf returned '" <> singleton x <> "' instead of a cardChar"

data ValueKind

foreign import data JacksNatural :: ValueKind
foreign import data JacksWild :: ValueKind

class JackValue :: ValueKind -> Constraint
class JackValue a where
  jackValue :: Int
  cardCounts :: Map Card Int -> Array Int

instance JackValue JacksNatural where
  jackValue = 0xB
  cardCounts = reverse <<< sort <<< fromFoldable <<< values

instance JackValue JacksWild where
  jackValue = 0x1
  cardCounts m = case pop Jack m of
    Nothing -> reverse $ sort $ fromFoldable $ values m
    Just (Tuple jacks m') ->
      case values m' of
        Nil -> [ jacks ]
        a -> over (ix 0) (_ + jacks) $ reverse $ sort $ fromFoldable a

handValue :: forall @a. JackValue a => Array Card -> Int
handValue cards = (pow 0x10 5) * handTypeValue h + cardsValue @a cards
  where
  m = foldr (\c -> insertWith (+) c 1) empty cards
  h = getHandType @a m

cardsValue :: forall @a. JackValue a => Array Card -> Int
cardsValue = unwrap <<< foldMapWithIndex calcCard <<< reverse
  where
  calcCard i c = Additive $ mul (cardValue @a c) (pow 0x10 i)

cardValue :: forall @a. JackValue a => Card -> Int
cardValue = case _ of
  Ace -> 0xE
  King -> 0xD
  Queen -> 0xC
  Jack -> jackValue @a
  Ten -> 0xA
  Nine -> 0x9
  Eight -> 0x8
  Seven -> 0x7
  Six -> 0x6
  Five -> 0x5
  Four -> 0x4
  Three -> 0x3
  Two -> 0x2

data HandType
  = FiveOAK
  | FourOAK
  | FullHouse
  | ThreeOAK
  | TwoPair
  | OnePair
  | HighCard
  | Invalid

getHandType :: forall @a. JackValue a => Map Card Int -> HandType
getHandType m = case cardCounts @a m of
  [ 5 ] -> FiveOAK
  [ 4, 1 ] -> FourOAK
  [ 3, 2 ] -> FullHouse
  [ 3, 1, 1 ] -> ThreeOAK
  [ 2, 2, 1 ] -> TwoPair
  [ 2, 1, 1, 1 ] -> OnePair
  [ 1, 1, 1, 1, 1 ] -> HighCard
  _ -> Invalid

handTypeValue :: HandType -> Int
handTypeValue = case _ of
  FiveOAK -> 0x7
  FourOAK -> 0x6
  FullHouse -> 0x5
  ThreeOAK -> 0x4
  TwoPair -> 0x3
  OnePair -> 0x2
  HighCard -> 0x1
  Invalid -> 0x0
