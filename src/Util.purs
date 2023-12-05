module AdventOfCode.Twenty23.Util
  ( lines
  , modify
  , skip
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
import Data.Symbol (class IsSymbol)
import Effect.Aff (Error)
import Parsing (Parser, runParser)
import Parsing.String (string)
import PointFree ((<..))
import Prim.Row (class Cons)
import Record as Rec
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

sumMap :: forall a. (a -> Int) -> Array a -> Int
sumMap = foldl add 0 <.. map

to2dArray :: String -> Array (Array Char)
to2dArray = map toCharArray <<< lines

lines :: String -> Array String
lines = split (Pattern "\n")

skip :: String -> Parser String Unit
skip = string >=> const (pure unit)

-- do
--   _ <- string s
--   pure unit

modify
  :: forall @l r1 r2 r a b
   . IsSymbol l
  => Cons l a r r1
  => Cons l b r r2
  => (a -> b)
  -> Record r1
  -> Record r2
modify = Rec.modify (Proxy :: _ l)

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

