module AdventOfCode.Twenty23.Util
  ( lines
  ) where

import Data.String (split)
import Data.String.Pattern (Pattern(..))

lines :: String -> Array String
lines = split (Pattern "\n")