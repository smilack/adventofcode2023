module AdventOfCode.Twenty23.Eight
  ( main
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Control.Alt ((<|>))
import Data.Functor (voidRight)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, fail)
import Parsing.Combinators (choice, try)
import Parsing.String (char, string)
import Parsing.String.Basic (oneOf)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/8"
  liftEffect do
    log "Part 1:"
    -- log ""
    -- logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

-- data Direction = L | R

-- newtype Path = Path (forall a. a -> Tuple Direction Path)

-- directionParser :: Parser String Direction
-- directionParser = do

