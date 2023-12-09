module AdventOfCode.Twenty23.Eight
  ( Direction(..)
  , Path(..)
  , main
  , mkPath
  , parseInput
  , parsePath
  , solve1
  -- , solve2
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
import Data.Map (Map, insert, keys, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (filter)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (toCharArray)
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

--     log "Minimum steps to reach __Z on all nodes"
--     logShow $ solve2 input

-- solve2 :: String -> Either ParseError Int
-- solve2 input = solve <$> runParser input parseInput
--   where
--   endsInA [_, _, 'A'] = true
--   endsInA _ = false
--   start = filter (toCharArray >>> endsInA) <<< keys

--   solve { path, nodes } =

--   go

solve1 :: String -> Either ParseError Int
solve1 input = go <$> runParser input parseInput
  where
  go { nodes, path } = followPath nodes path "AAA" 0

followPath :: Map String Node -> Path -> String -> Int -> Int
followPath nodeMap path node i
  | node == "ZZZ" = i
  | otherwise =
      case next nodeMap path node of
        Nothing -> i
        Just { path', nextNode } ->
          followPath nodeMap path' nextNode (inc i)

next
  :: Map String Node
  -> Path
  -> String
  -> Maybe { path' :: Path, nextNode :: String }
next nodeMap (Path path) node = do
  { left, right } <- lookup node nodeMap
  { head, tail } <- uncons path
  let
    nextNode = case head of
      L -> left
      R -> right
  pure $ { nextNode, path': Path tail }

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