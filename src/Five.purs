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
  , Map2
  , Range
  , SeedRange
  , contains
  , validSeed
  , seedRangeParser
  ) where

import Prelude

import AdventOfCode.Twenty23.Util (between', inc, range', skip)
import Data.Array.NonEmpty (NonEmptyArray, any, concat)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, modify, unwrap)
import Data.Semigroup.Foldable (minimum)
import Data.Traversable (oneOf)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators.Array (many1)
import Parsing.String (anyTill, string)
import Parsing.String.Basic (number, skipSpaces)

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

solve2 :: String -> Either ParseError Number
solve2 input = do
  seeds <- runParser input seedRangeParser
  almanac <- runParser input parseMapsR
  pure $ search (Id 0.0) seeds almanac
  where
  search :: Id Location -> NonEmptyArray SeedRange -> Map Location Seed -> Number
  search loc seeds almanac
    | validSeed (almanac loc) seeds = unwrap loc
    | otherwise = search (modify inc loc) seeds almanac

seedRangeParser :: Parser String (NonEmptyArray SeedRange)
seedRangeParser = do
  skip "seeds:"
  many1 do
    skipSpaces
    start <- number
    skipSpaces
    len <- number
    pure { start, len }

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

solve1 :: String -> Either ParseError Number
solve1 input = do
  seeds <- runParser input seedParser
  almanac <- runParser input parseMaps
  let
    locations = map (unwrap <<< almanac) seeds
  pure $ minimum locations

type Map2 :: GardenKind -> GardenKind -> Type
type Map2 a b = NonEmptyArray (Range a b)

type Range :: GardenKind -> GardenKind -> Type
type Range a b = { source :: Id a, dest :: Id b, len :: Number }

type SeedRange = { start :: Number, len :: Number }

contains :: Number -> SeedRange -> Boolean
contains a { start, len } = between' start len a

validSeed :: Id Seed -> NonEmptyArray SeedRange -> Boolean
validSeed = any <<< contains <<< unwrap

-- type Transformation = { start :: Number, len :: Number, change :: Number }

-- transform 

-- combinable :: forall a b. Range a b -> Range a b -> Boolean
-- combinable a b = adjacent a b || overlapping a b

-- overlapping :: forall a b. Range a b -> Range a b -> Boolean
-- overlapping a b =

-- adjacent :: forall a b. Range a b -> Range a b -> Boolean
-- a b =

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

mapParserR :: forall @f @t. Reflect f => Reflect t => Parser String (Map f t)
mapParserR = do
  _ <- anyTill $ string $ mapName @t @f
  mappings <- many1 parseOneMappingR
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

parseOneMappingR :: Parser String Mapping
parseOneMappingR = do
  skipSpaces
  destStart <- number
  skipSpaces
  sourceStart <- number
  skipSpaces
  rangeLength <- number
  pure $ mkOneMapping sourceStart destStart rangeLength

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

parseMapsR :: Parser String (Map Location Seed)
parseMapsR = do
  s <- mapParserR @Soil @Seed
  f <- mapParserR @Fertilizer @Soil
  w <- mapParserR @Water @Fertilizer
  l <- mapParserR @Light @Water
  t <- mapParserR @Temperature @Light
  h <- mapParserR @Humidity @Temperature
  o <- mapParserR @Location @Humidity
  pure
    ( s
        <<< f
        <<< w
        <<< l
        <<< t
        <<< h
        <<< o
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

