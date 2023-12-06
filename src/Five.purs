module AdventOfCode.Twenty23.Five
  ( Fertilizer
  , GardenKind(..)
  , Humidity
  , Id(..)
  , Light
  , Location
  , Map
  , Seed
  , Soil
  , Temperature
  , Water
  , class Reflect
  , main
  , mapParser
  , mapName
  , name
  , parseMaps
  , seedParser
  , seedParser2
  , mkOneMapping
  , parseOneMapping
  , solve1
  , solve2
  , mkMap
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Data.Array (findMap, head)
import Data.Array.NonEmpty (NonEmptyArray, concat, fromArray, range)
import Data.Either (Either(..), fromRight)
import Data.Function (applyFlipped)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Reflectable (reflectType)
import Data.Semigroup.Foldable (minimum)
import Data.Traversable (oneOf, sequence)
import Debug (spy, trace)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators.Array (many, many1)
import Parsing.String (anyTill, string)
import Parsing.String.Basic (number, skipSpaces)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/5"
  liftEffect do
    log "Part 1:"
    log "Lowest location number:"
    logShow $ solve1 input
    log "Part2:"
    log "Lowest location number:"
    logShow $ solve2 input

solve2 :: String -> Number
solve2 input =
  case result of
    Left er -> trace er (const 0.0)
    Right n -> n
  where
  result = do
    seeds <- runParser input seedParser2
    almanac <- runParser input parseMaps
    let
      locations = map (unwrap <<< almanac) seeds
    pure $ minimum locations

seedParser2 :: Parser String (NonEmptyArray (Id Seed))
seedParser2 = do
  skip "seeds:"
  ranges <- many1 do
    skipSpaces
    start <- number
    skipSpaces
    length <- number
    pure $ range' start length
  pure $ map Id $ concat ranges

solve1 :: String -> Number
solve1 input =
  case result of
    Left er -> trace er (const 0.0)
    Right n -> n
  where
  result = do
    seeds <- runParser input seedParser
    almanac <- runParser input parseMaps
    let
      locations = map (unwrap <<< almanac) seeds
    pure $ minimum locations

type Map :: GardenKind -> GardenKind -> Type
type Map a b = Id a -> Id b

newtype Id :: GardenKind -> Type
newtype Id a = Id Number

derive instance Newtype (Id a) _
derive instance Eq (Id a)

instance Reflect a => Show (Id a) where
  show (Id id) = name @a <> " " <> show id

seedParser :: Parser String (NonEmptyArray (Id Seed))
seedParser = do
  skip "seeds:"
  many1 do
    skipSpaces
    map (Id @Seed) number

mapParser :: forall @f @t. Reflect f => Reflect t => Parser String (Map f t)
mapParser = do
  _ <- anyTill $ string $ mapName @f @t
  mappings <- many1 parseOneMapping
  pure $ mkMap mappings

mkMap :: forall @f @t. Reflect f => Reflect t => NonEmptyArray Mapping -> Map f t
mkMap mappings (Id a) = Id @t $ fromMaybe a $ oneOf $ flap mappings a

mapName :: forall @f @t. Reflect f => Reflect t => String
mapName = name @f <> "-to-" <> name @t <> " map:"

type Mapping = Number -> Maybe Number

parseOneMapping :: Parser String Mapping
parseOneMapping = do
  skipSpaces
  destStart <- number
  skipSpaces
  sourceStart <- number
  skipSpaces
  rangeLength <- number
  pure $ mkOneMapping destStart sourceStart rangeLength

mkOneMapping :: Number -> Number -> Number -> Number -> Maybe Number
mkOneMapping destStart sourceStart rangeLength test =
  if between' sourceStart rangeLength test then
    Just $ test - sourceStart + destStart
  else
    Nothing

parseMaps :: Parser String (Map Seed Location)
parseMaps = do
  s <- mapParser @Seed @Soil
  f <- mapParser @Soil @Fertilizer
  w <- mapParser @Fertilizer @Water
  l <- mapParser @Water @Light
  t <- mapParser @Light @Temperature
  h <- mapParser @Temperature @Humidity
  o <- mapParser @Humidity @Location
  pure
    ( s
        >>> f
        >>> w
        >>> l
        >>> t
        >>> h
        >>> o
    )

-------------------------
---- fancy type junk ----
-------------------------
data GardenKind

class Reflect :: GardenKind -> Constraint
class Reflect a where
  name :: String

foreign import data Seed :: GardenKind
foreign import data Soil :: GardenKind
foreign import data Fertilizer :: GardenKind
foreign import data Water :: GardenKind
foreign import data Light :: GardenKind
foreign import data Temperature :: GardenKind
foreign import data Humidity :: GardenKind
foreign import data Location :: GardenKind

instance Reflect Seed where
  name = "seed"

instance Reflect Soil where
  name = "soil"

instance Reflect Fertilizer where
  name = "fertilizer"

instance Reflect Water where
  name = "water"

instance Reflect Light where
  name = "light"

instance Reflect Temperature where
  name = "temperature"

instance Reflect Humidity where
  name = "humidity"

instance Reflect Location where
  name = "location"

