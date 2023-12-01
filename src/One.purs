module AdventOfCode.Twenty23.One where

import Prelude
import AdventOfCode.Twenty23.Util
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/1"
  liftEffect do
    log "Part 1:"
    -- log ""
    -- logShow $ solve1 input
    log "Part2:"
    -- log ""
    -- logShow $ solve2 input