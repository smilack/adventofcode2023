module AdventOfCode.Twenty23.Nine
  ( main
  , parseHistories
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Data.List (List)
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

