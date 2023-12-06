module AdventOfCode.Twenty23.Util
  ( between'
  , lines
  , modify
  , range'
  , skip
  , sumMap
  , testParser
  , to2dArray
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (cons, foldl)
import Data.Array.NonEmpty (NonEmptyArray, cons')
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

-- Test if a number is within `length` numbers of `start`. `start` counts as 1
-- `between' 10 0 x` is like `x in []`
-- `between' 10 1 x` is like `x in [10]`
-- `between' 10 2 x` is like `x in [10, 11]`
-- `between' 10 5 x` is like `x in [10, 11, 12, 13, 14]`
between' :: forall a. Ord a => Ring a => a -> a -> a -> Boolean
between' start length
  | length <= zero = const false
  | otherwise = between start (start + length - one)

range' :: forall a. Ord a => Ring a => a -> a -> NonEmptyArray a
range' start length = cons' start $ go (start + one) (length - one)
  where
  go n l
    | l <= zero = []
    | otherwise = cons n $ go (n + one) (l - one)

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

