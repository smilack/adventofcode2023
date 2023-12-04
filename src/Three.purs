module AdventOfCode.Twenty23.Three
  ( Location
  , main
  , parseInput1
  , region
  , solve1
  , symbolInRegion
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Data.Array (any, concatMap, filter, foldl, slice)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.String (codePointAt, codePointFromChar, split)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, Position(..), position, runParser)
import Parsing.Combinators.Array (many)
import Parsing.String (anyTill)
import Parsing.String.Basic (intDecimal)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/3"
  liftEffect do
    log "Part 1:"
    log "Sum of numbers adjacent to symbols"
    logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

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