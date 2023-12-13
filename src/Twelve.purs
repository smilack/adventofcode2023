module AdventOfCode.Twenty23.Twelve
  ( Rec
  , Spring(..)
  , countPossibilities
  , main
  , parseRecords
  , solve1
  , solve2
  , unfold
  , validate
  ) where

import AdventOfCode.Twenty23.Util (inc, sumMap)
import Prelude

import Control.Alt ((<|>))
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (all, foldMap, intercalate)
import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import JS.BigInt (BigInt, fromInt)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (many, replicateA, sepBy, sepEndBy, skipMany, skipMany1, (<?>))
import Parsing.String (char)
import Parsing.String.Basic (intDecimal)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/12"
  liftEffect do
    log "Part 1:"
    log "Number of possible arrangements"
    logShow $ solve1 input
    log "Part2:"
    log "Number of possible arrangements after quintupling"
    logShow $ solve2 input

solve2 :: String -> Either ParseError BigInt
solve2 input =
  sumMap extrapolatePossibilities
    <$> runParser input parseRecords

unfold :: Int -> Rec -> Rec
unfold n { springs, groups } =
  { springs: intercalate (Unknown : Nil)
      $ (replicate n springs :: List (List Spring))
  , groups: join $ replicate n groups
  }

extrapolatePossibilities :: Rec -> BigInt
extrapolatePossibilities rec = n * m4
  where
  n = fromInt $ countPossibilities rec
  m =
    ( fromInt $ countPossibilities (unfold 2 rec)
    ) / n
  m4 = m * m * m * m

solve1 :: String -> Either ParseError Int
solve1 input =
  sumMap countPossibilities
    <$> runParser input parseRecords

type Rec =
  { groups :: List Int
  , springs :: List Spring
  }

parseRecords :: Parser String (List Rec)
parseRecords = parseRecord `sepEndBy` char '\n'

parseRecord :: Parser String Rec
parseRecord = do
  springs <- parseSprings
  _ <- char ' '
  groups <- intDecimal `sepBy` char ','
  pure $ { groups, springs }

parseSprings :: Parser String (List Spring)
parseSprings = many $
  Bad <$ char '#'
    <|> Good <$ char '.'
    <|> Unknown <$ char '?'
      <?> "Spring (one of .#?)"

validate :: Rec -> Boolean
validate { springs, groups } =
  case
    runParser (foldMap show springs)
      $ go skipMany groups
    of
    Left _ -> false
    Right b -> b
  where
  go :: (Parser String Char -> Parser String Unit) -> List Int -> Parser String Boolean
  go _ Nil = pure true
  go skipFn (g : gs) = skipFn good *> bad g *> go skipMany1 gs

  good :: Parser String Char
  good = char '.'

  bad :: Int -> Parser String (List Char)
  bad n = replicateA n $ char '#'

data Streak = None | Active Int

data Spring = Good | Bad | Unknown

derive instance Eq Spring

instance Show Spring where
  show Good = "."
  show Bad = "#"
  show Unknown = "?"

countPossibilities :: Rec -> Int
countPossibilities rec =
  go None rec.springs rec.groups
  where
  goodOrUnknown s = s `elem` [ Good, Unknown ]

  go :: Streak -> List Spring -> List Int -> Int
  go streak springs groups =
    case springs /\ groups of
      Nil /\ Nil -> 1
      Nil /\ (g : Nil) -> case streak of
        None -> 0
        Active n -> fromEnum $ g == n
      Nil /\ _ -> 0
      ss /\ Nil -> fromEnum $ all goodOrUnknown ss
      (s : springs') /\ (g : groups') ->
        case s of
          Good -> case streak of
            None -> go None springs' groups
            Active n ->
              if n == g then
                go None springs' groups'
              else
                0
          Bad -> case streak of
            None -> go (Active 1) springs' groups
            Active n ->
              if n <= g then
                go (Active (inc n)) springs' groups
              else
                0
          Unknown -> add
            (go streak (Bad : springs') groups)
            (go streak (Good : springs') groups)