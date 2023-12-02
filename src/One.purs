module AdventOfCode.Twenty23.One
  ( getNumber
  , main
  , parseInput1
  , parseInput2
  , solve1
  , solve2
  ) where

import AdventOfCode.Twenty23.Util (lines)
import Prelude

import Control.Alt ((<|>))
import Data.Array (filter, head, reverse)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (singleton, toCharArray, uncons)
import Data.String.Utils (startsWith)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many)
import Parsing.String (anyTill, consumeWith)
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
solve1 = sum <<< map getNumber <<< parseInput1

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
  digitParser = try digit <|> try (consumeWith checkSpelled)

checkSpelled
  :: String
  -> Either
       String
       { consumed :: String, remainder :: String, value :: Char }
checkSpelled s = result
  where
  result
    | startsWith "one" s = Right $ rec "o" '1'
    | startsWith "two" s = Right $ rec "t" '2'
    | startsWith "three" s = Right $ rec "t" '3'
    | startsWith "four" s = Right $ rec "f" '4'
    | startsWith "five" s = Right $ rec "f" '5'
    | startsWith "six" s = Right $ rec "s" '6'
    | startsWith "seven" s = Right $ rec "s" '7'
    | startsWith "eight" s = Right $ rec "e" '8'
    | startsWith "nine" s = Right $ rec "n" '9'
    | otherwise = Left "String does not start with a spelled digit"
  rec =
    { consumed: _
    , remainder: fromMaybe "" $ map (_.tail) $ uncons s
    , value: _
    }