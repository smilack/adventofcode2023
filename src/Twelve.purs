module AdventOfCode.Twenty23.Twelve
  ( Rec
  , main
  , parseRecords
  , validate
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (replicateA, sepBy, sepEndBy, skipMany, skipMany1)
import Parsing.String (anyTill, char)
import Parsing.String.Basic (intDecimal)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/12"
  liftEffect do
    log "Part 1:"
    -- log ""
    -- logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

type Rec = { rec :: String, groups :: List Int }

parseRecords :: Parser String (List Rec)
parseRecords = parseRecord `sepEndBy` char '\n'

parseRecord :: Parser String Rec
parseRecord = do
  (rec /\ _) <- anyTill $ char ' '
  groups <- intDecimal `sepBy` char ','
  pure $ { rec, groups }

validate :: Rec -> Boolean
validate { rec, groups } =
  case runParser rec $ go skipMany groups of
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