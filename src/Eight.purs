module AdventOfCode.Twenty23.Eight
  ( Direction(..)
  , Path(..)
  , main
  , parsePath
  , mkPath
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.List.Lazy as Lazy
import Data.Map (Map)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser)

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

newtype Path = Path (Lazy.List Direction)

derive instance Newtype Path _

instance Show Path where
  show = (_ <> "...") <<< Lazy.foldMap show <<< Lazy.take 10 <<< unwrap

parsePath :: Parser String Path
parsePath = mkPath <$> Lazy.many (genericParser @Direction)

mkPath :: Lazy.List Direction -> Path
mkPath = Path <<< Lazy.cycle

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