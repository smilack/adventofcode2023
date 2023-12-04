module AdventOfCode.Twenty23.Util
  ( lines
  , sumMap
  , testParser
  , to2dArray
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (foldl)
import Data.Either (Either(..))
import Data.String (split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Error)
import Parsing (Parser, runParser)
import PointFree ((<..))
import Test.Spec.Assertions (shouldEqual)

sumMap :: forall a. (a -> Int) -> Array a -> Int
sumMap = foldl add 0 <.. map

to2dArray :: String -> Array (Array Char)
to2dArray = map toCharArray <<< lines

lines :: String -> Array String
lines = split (Pattern "\n")

testParser
  :: forall m a
   . MonadThrow Error m
  => Show a
  => Eq a
  => String
  -> a
  -> Parser String a
  -> m Unit
testParser input expected parser =
  runParser input parser `shouldEqual` Right expected

