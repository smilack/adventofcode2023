module AdventOfCode.Twenty23.Three
  ( Location
  , gearRatio
  , includes
  , locateStars
  , main
  , parseInput1
  , region
  , solve1
  , solve2
  , symbolInRegion
  ) where

import AdventOfCode.Twenty23.Util (to2dArray)
import Prelude

import Data.Array (any, concatMap, filter, foldl, slice)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Ord (abs)
import Data.String (codePointFromChar)
import Data.String as String
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, Position(..), position, runParser)
import Parsing.Combinators.Array (many)
import Parsing.String (anyTill, char)
import Parsing.String.Basic (intDecimal)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/3"
  liftEffect do
    log "Part 1:"
    log "Sum of numbers adjacent to symbols"
    logShow $ solve1 input
    log "Part2:"
    log "Sum of (product of pairs of numbers adjacent to *)"
    logShow $ solve2 input

type Point = { line :: Int, column :: Int }

type Location = { line :: Int, column :: Int, length :: Int, number :: Int }

symbolInRegion :: Array Char -> Boolean
symbolInRegion = any isSymbol

isSymbol :: Char -> Boolean
isSymbol c = notDigit && notPeriod
  where
  notDigit = not $ isDecDigit $ codePointFromChar c
  notPeriod = c /= '.'

region
  :: forall a
   . Array (Array a)
  -> Location
  -> Array a
region array { line, column, length } =
  concatMap (slice (max 0 from.column) to.column)
    $ slice (max 0 from.line) to.line array
  where
  from =
    { column: max 0 (column - 1)
    , line: max 0 (line - 1)
    }
  to =
    { column: column + length + 1
    , line: line + 2
    }

parseInput1 :: Parser String (Array Location)
parseInput1 = many locateNumber

locateNumber :: Parser String Location
locateNumber = do
  number <- anyTill intDecimal >>= \t -> pure $ snd t
  { line, column } <- position >>= \(Position p) -> pure p
  let
    length = String.length $ show number
  pure { line: line - 1, column: column - length - 1, length, number: abs number }

solve1 :: String -> Int
solve1 input = case runParser input parseInput1 of
  Left _ -> 0
  Right numbers ->
    foldl (+) 0
      $ map _.number
      $ filter (symbolInRegion <<< (region chars)) numbers
  where
  chars = to2dArray input

locateStars :: Parser String (Array Point)
locateStars = many locateStar
  where
  locateStar = do
    _ <- anyTill $ char '*'
    p <- position >>= \(Position p) -> pure p
    pure { line: p.line - 1, column: p.column - 2 }

solve2 :: String -> Int
solve2 input = case runParser input parseInput1 of
  Left _ -> 0
  Right numbers ->
    case runParser input locateStars of
      Left _ -> 0
      Right stars ->
        sum $ map (gearRatio <<< \s -> filter (includes s) numbers) $ stars

includes :: Point -> Location -> Boolean
includes p l = (betweenCols p.column) && (betweenLines p.line)
  where
  betweenCols = between (l.column - 1) (l.column + l.length)
  betweenLines = between (l.line - 1) (l.line + 1)

gearRatio :: forall r. Array { number :: Int | r } -> Int
gearRatio = case _ of
  [ l1, l2 ] -> l1.number * l2.number
  _ -> 0