module AdventOfCode.Twenty23.Ten
  ( Direction(..)
  , Grid(..)
  , Pipe(..)
  , countStepsInLoop
  , main
  , parseGrid
  , parsePipe
  , startLocation
  , validMoves
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array (filter)
import Data.Array.NonEmpty (NonEmptyArray, elem, elemIndex, findIndex, foldMap1, fromFoldable1, intercalate, (!!))
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser)
import Parsing.Combinators (sepEndBy1, (<?>))
import Parsing.Combinators.Array (many1)
import Parsing.String (char)
import Parsing.String.Basic (skipSpaces)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/10"
  liftEffect do
    log "Part 1:"
    -- log ""
    -- logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

-- Each loop of pipes will always have an even
-- number of segments. Therefore the number of
-- steps to the farthest segment from the start
-- is half the length of the loop.

type Coord = { x :: Int, y :: Int }

countStepsInLoop :: Grid -> Int
countStepsInLoop (Grid g) = 0

startLocation :: Grid -> Maybe Coord
startLocation (Grid g) = lift2 { x: _, y: _ } x y
  where
  y = findIndex (S `elem` _) g
  x = elemIndex S =<< (g !! _) =<< y

parseGrid :: Parser String Grid
parseGrid = map
  (Grid <<< fromFoldable1)
  (skipSpaces *> parseLines)
  where
  parseLines = many1 parsePipe `sepEndBy1` char '\n'

parsePipe :: Parser String Pipe
parsePipe = Г <$ char 'F'
  <|> 𝖨 <$ char '|'
  <|> L <$ char 'L'
  <|> 𐐑 <$ char '7'
  <|> 𐐇 <$ char 'J'
  <|> Ⲻ <$ char '-'
  <|> O <$ char '.'
  <|> S <$ char 'S'
    <?> "Pipe (one of: F|L7J-.S)"

newtype Grid = Grid (NonEmptyArray (NonEmptyArray Pipe))

derive instance Eq Grid

instance Show Grid where
  show (Grid g) = "\n"
    <> (intercalate "\n" $ map (foldMap1 show) g)
    <> "\n"

-- just being silly
data Pipe
  = Г -- U+0413 Cyrillic Capital Letter Ghe
  | 𝖨 -- U+1D5A8 Mathematical Sans-Serif Capital I
  | L -- just L
  | 𐐑 -- U+10411 Deseret Capital Letter Pee
  | 𐐇 -- U+10407 Deseret Capital Letter Short E
  | Ⲻ -- U+2CBA Coptic Capital Letter Dialect-P Ni
  | O -- just O
  | S -- just S

derive instance Eq Pipe

instance Show Pipe where
  show Г = "╔"
  show 𝖨 = "║"
  show L = "╚"
  show 𐐑 = "╗"
  show 𐐇 = "╝"
  show Ⲻ = "═"
  show O = " "
  show S = "⬤"

data Direction
  = Up
  | Dn
  | Rt
  | Lf

derive instance Eq Direction

instance Show Direction where
  show Up = "🠙"
  show Dn = "🠛"
  show Rt = "🠚"
  show Lf = "🠘"

validMoves :: Grid -> Pipe -> Maybe (Tuple Direction Direction)
validMoves g = case _ of
  Г -> Just (Tuple Rt Dn)
  𝖨 -> Just (Tuple Dn Up)
  L -> Just (Tuple Rt Up)
  𐐑 -> Just (Tuple Dn Lf)
  𐐇 -> Just (Tuple Lf Up)
  Ⲻ -> Just (Tuple Rt Lf)
  O -> Nothing
  S -> startMoves
  where
  startMoves = startLocation g >>= \loc ->
    let
      valid = filter
        (pointsBackTo loc g)
        [ Up, Dn, Rt, Lf ]
    in
      case valid of
        [ a, b ] -> Just (Tuple a b)
        _ -> Nothing

pointsBackTo :: Coord -> Grid -> Direction -> Boolean
pointsBackTo c g d = isJust do
  p <- get (move c d) g
  (Tuple a b) <- validMoves g p
  pure $ opposites a d || opposites b d

opposites :: Direction -> Direction -> Boolean
opposites Up Dn = true
opposites Dn Up = true
opposites Rt Lf = true
opposites Lf Rt = true
opposites _ _ = false

move :: Coord -> Direction -> Coord
move { x, y } = case _ of
  Up -> { x, y: y - 1 }
  Dn -> { x, y: y + 1 }
  Rt -> { x: x + 1, y }
  Lf -> { x: x - 1, y }

get :: Coord -> Grid -> Maybe Pipe
get { x, y } (Grid g) = (g !! y) >>= (\row -> row !! x)

