module AdventOfCode.Twenty23.One
  ( getNumber
  , main
  , parseInput1
  , parseInput2
  , solve1
  , solve2
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Control.Alt ((<|>))
import Data.Array (filter, head, reverse)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromMaybe)
import Data.String (CodePoint, split)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (many1, try)
import Parsing.Combinators.Array (many)
import Parsing.String (anyChar, anyCodePoint, anyTill, string)
import Parsing.String.Basic (digit)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/1"
  liftEffect do
    log "Part 1:"
    log "Sum of numbers"
    logShow $ solve1 input
    log "Part2:"
    log "Sum of numbers"
    logShow $ solve2 input
    _ <- traverse
      ( \line -> liftEffect do
          log line
          let p = parseInput2 line
          logShow p
          let n = getNumber p
          logShow n
      )
      (lines input)
    pure unit

parseInput1 :: String -> Array (Array Char)
parseInput1 =
  map (filter (isDecDigit <<< codePointFromChar))
    <<< map toCharArray
    <<< lines

getNumber :: Array Char -> Int
getNumber array = fromMaybe 0 $ do
  first <- head array
  last <- head $ reverse array
  number <- fromString $ singleton first <> singleton last
  pure number

solve1 :: String -> Int
solve1 =
  sum
    <<< map getNumber
    <<< parseInput1

solve2 :: String -> Int
solve2 = sum <<< map getNumber <<< map parseInput2 <<< lines

parseInput2 :: String -> Array Char
parseInput2 i =
  case runParser i parser of
    Left _ -> []
    Right a -> a
  where
  parser :: Parser String (Array Char)
  parser = do
    digitTuples <- many $ anyTill digitParser
    let
      digits = map snd digitTuples
    pure digits

  digitParser :: Parser String Char
  digitParser =
    try digit
      <|> try (string "one" >>= const (pure '1'))
      <|> try (string "two" >>= const (pure '2'))
      <|> try (string "three" >>= const (pure '3'))
      <|> try (string "four" >>= const (pure '4'))
      <|> try (string "five" >>= const (pure '5'))
      <|> try (string "six" >>= const (pure '6'))
      <|> try (string "seven" >>= const (pure '7'))
      <|> try (string "eight" >>= const (pure '8'))
      <|> try (string "nine" >>= const (pure '9'))