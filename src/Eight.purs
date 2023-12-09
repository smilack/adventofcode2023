module AdventOfCode.Twenty23.Eight
  ( Direction(..)
  , Path(..)
  , main
  , mkPath
  , parseInput
  , parsePath
  , solve1
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Control.Plus (empty)
import Data.Array (foldr, many)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.List.Lazy (nil, uncons, (:))
import Data.List.Lazy as Lazy
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError(..), Parser, runParser)
import Parsing.Combinators (try)
import Parsing.String (takeN)
import Parsing.String.Basic (skipSpaces)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/8"
  liftEffect do
    log "Part 1:"
    log "Minimum steps to reach ZZZ"
    logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

solve1 :: String -> Either ParseError Int
solve1 input = start <$> runParser input parseInput
  where
  start { nodes, path } = go 0 "AAA" path nodes

  go :: Int -> String -> Path -> Map String Node -> Int
  go i "ZZZ" _ _ = i -- reached the end
  go i node (Path path) nodes = fromMaybe i do
    { left, right } <- lookup node nodes
    { head, tail } <- uncons path
    let
      next = case head of
        L -> left
        R -> right
    pure $ go (inc i) next (Path tail) nodes

parseInput :: Parser String { nodes :: Map String Node, path :: Path }
parseInput = do
  path <- parsePath
  nodes <- parseNodes
  pure $ { path, nodes }

data Direction = L | R

newtype Path = Path (Lazy.List Direction)

derive instance Newtype Path _

instance Show Path where
  show = (_ <> "...") <<< Lazy.foldMap show <<< Lazy.take 10 <<< unwrap

parsePath :: Parser String Path
parsePath = mkPath <$> Lazy.many (genericParser @Direction)

mkPath :: Lazy.List Direction -> Path
mkPath = Path <<< Lazy.cycle

type Node = { name :: String, left :: String, right :: String }

parseNodes :: Parser String (Map String Node)
parseNodes = do
  nodes <- many $ try parseNode
  pure $ foldr insertNode empty nodes
  where
  insertNode node@{ name } = insert name node

parseNode :: Parser String Node
parseNode = do
  skipSpaces
  name <- takeN 3
  skip " = ("
  left <- takeN 3
  skip ", "
  right <- takeN 3
  skip ")"
  pure { name, left, right }

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