module AdventOfCode.Twenty23.Nine
  ( derivative
  , main
  , parseHistories
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Data.List (List(..), scanl, uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser)
import Parsing.Combinators (sepBy)
import Parsing.String (char)
import Parsing.String.Basic (intDecimal, skipSpaces)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/9"
  liftEffect do
    log "Part 1:"
    -- log ""
    -- logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

parseHistories :: Parser String (List (List Int))
parseHistories = parseHistory `sepBy` char '\n'

parseHistory :: Parser String (List Int)
parseHistory = intDecimal `sepBy` char ' '

--findConstFn :: List (List a) -> List (List a)
-- findConstFn fnfam
--   | all lastOf 0 =
-- | otherwise = findConstFn $ fnfam snoc (derivative $ lastOf fnfam)

derivative :: forall a. Ring a => List a -> List a
derivative = map (\(Tuple a b) -> b - a) <<< pairs

pairs :: forall a. List a -> List (Tuple a a)
pairs = uncons >>> case _ of
  Nothing -> Nil
  Just { head, tail } ->
    scanl pair (Tuple head head) tail
  where
  pair (Tuple _ prev) next = Tuple prev next