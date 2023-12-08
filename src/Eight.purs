module AdventOfCode.Twenty23.Eight
  ( Direction(..)
  , Path(..)
  , main
  , parsePath
  , mkPath
  , iteratePath
  , next
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Control.Alt ((<|>))
import Data.Array (cons, foldMap)
import Data.Array.NonEmpty (NonEmptyArray, snoc', uncons)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum, cardinality, enumFromTo)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Functor (voidRight)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), fst)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, fail)
import Parsing.Combinators (choice, try)
import Parsing.Combinators.Array (many1)
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

data Direction = L | R

newtype Path = Path (forall a. a -> Tuple Direction Path)

next :: Path -> Tuple Direction Path
next (Path f) = f unit

iteratePath :: Int -> Path -> Array Direction
iteratePath n p
  | n <= 0 = []
  | otherwise =
      let
        (Tuple d p') = next p
      in
        cons d $ iteratePath (dec n) p'

instance Show Path where
  show = (_ <> "...") <<< foldMap show <<< iteratePath 10

parsePath :: Parser String Path
parsePath = map mkPath $ many1 $ genericParser @Direction

mkPath :: NonEmptyArray Direction -> Path
mkPath nea = Path
  ( \_ ->
      let
        { head, tail } = uncons nea
        nea' = snoc' tail head
      in
        Tuple head (mkPath nea')
  )

derive instance Eq Direction
derive instance Ord Direction
derive instance Generic Direction _
instance Show Direction where
  show = genericShow

instance Enum Direction where
  succ = genericSucc
  pred = genericPred

instance Bounded Direction where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Direction where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum