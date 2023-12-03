module AdventOfCode.Twenty23.Two
  ( Bag
  , Colors
  , Game
  , GameSpec
  , Handful(..)
  , bag
  , isPossible
  , main
  , parseColor
  , parseGame
  , power
  , toGame
  ) where

import AdventOfCode.Twenty23.Util (lines)
import Prelude

import Control.Alt ((<|>))
import Data.Array (filter)
import Data.Either (fromRight)
import Data.Foldable (foldl, sum)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, modify)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy, try)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, oneOf, skipSpaces)
import Record (insert)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/2"
  liftEffect do
    log "Part 1:"
    log "Sum of ids of possible games"
    logShow $ solve1 input
    log "Part2:"
    log "Sums power of all games"
    logShow $ solve2 input

parse :: String -> Array Game
parse =
  map (fromRight empty)
    <<< map (\s -> runParser s parseGame)
    <<< lines

solve1 :: String -> Int
solve1 =
  sum
    <<< map _.id
    <<< filter (isPossible bag)
    <<< parse

solve2 :: String -> Int
solve2 =
  sum
    <<< map power
    <<< parse

type Colors s =
  ( r :: Int
  , g :: Int
  , b :: Int
  | s
  )

type GameSpec = Record (Colors ())

power :: forall r. Record (Colors r) -> Int
power { r, g, b } = r * g * b

type Game = Record (Colors (id :: Int))

newtype Handful = Handful GameSpec

derive instance Newtype Handful _

derive instance Eq Handful

derive instance Generic Handful _

instance Show Handful
  where
  show = genericShow

instance Semigroup Handful where
  append (Handful a) (Handful b) =
    Handful
      { r: max a.r b.r
      , g: max a.g b.g
      , b: max a.b b.b
      }

instance Monoid Handful where
  mempty =
    Handful { r: 0, g: 0, b: 0 }

empty :: Game
empty = toGame 0 mempty

parseGame :: Parser String Game
parseGame = do
  _ <- string "Game "
  id <- intDecimal
  _ <- string ":"
  handfuls <- parseColor `sepBy` parseSeparator
  let
    handful = foldl append mempty handfuls
  pure $ toGame id handful

toGame :: Int -> Handful -> Game
toGame id (Handful h) = insert (Proxy :: Proxy "id") id h

parseSeparator :: Parser String Char
parseSeparator = do
  skipSpaces
  oneOf [ ',', ';' ]

parseColor :: Parser String Handful
parseColor = do
  skipSpaces
  n <- intDecimal
  skipSpaces
  color <- try (string "red") <|> try (string "green") <|> try (string "blue")
  let
    update = case color of
      "red" -> _ { r = n }
      "green" -> _ { g = n }
      "blue" -> _ { b = n }
      _ -> identity
  pure $ modify update mempty

type Bag = GameSpec

isPossible :: Bag -> Game -> Boolean
isPossible b g = (b.r >= g.r) && (b.g >= g.g) && (b.b >= g.b)

bag :: Bag
bag = { r: 12, g: 13, b: 14 }