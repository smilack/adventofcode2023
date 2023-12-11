module AdventOfCode.Twenty23.Ten
  ( Direction(..)
  , Grid(..)
  , InOrOut(..)
  , Pipe(..)
  , countStepsInLoop
  , emptyGridMap
  , get
  , main
  , mapPipeEdges
  , move
  , opposite
  , parseGrid
  , parsePipe
  , pointsBackTo
  , solve1
  , startLocation
  , validMoves
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty, guard)
import Control.Apply (lift2)
import Data.Array (filter, head, (:))
import Data.Array.NonEmpty (NonEmptyArray, elem, elemIndex, findIndex, foldMap1, fromFoldable1, intercalate, length, replicate, uncons, (!!))
import Data.Either (Either)
import Data.Foldable (foldMap, or)
import Data.Generic.Rep (class Generic)
import Data.Lens.Index (ix)
import Data.Lens.Types (AffineTraversal')
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid.Disj (Disj(..))
import Data.Tuple (Tuple(..))
import Debug (spy, spyWith)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError(..), Parser, runParser)
import Parsing.Combinators (sepEndBy1, (<?>))
import Parsing.Combinators.Array (many1)
import Parsing.String (char)
import Parsing.String.Basic (skipSpaces)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/10"
  liftEffect do
    log "Part 1:"
    log "Half loop length"
    logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

-- Each loop of pipes will always have an even
-- number of segments. Therefore the number of
-- steps to the farthest segment from the start
-- is half the length of the loop.

type Coord = { x :: Int, y :: Int }

emptyGridMap :: Grid Pipe -> Grid InOrOut
emptyGridMap (Grid nea) = Grid $
  replicate (length nea)
    (replicate (length head) Unknown)
  where
  { head } = uncons nea

mapPipeEdges :: Grid Pipe -> Grid InOrOut
mapPipeEdges g =
  -- traverse through pipe as in countStepsInLoop
  -- set corners to Corner
  -- for horizontal:
  --   if moving L->R set to EdgeDown
  --             L<-R EdgeUp
  -- vertical:
  --   moving U->D EdgeLeft
  --          U<-D EdgeRight
  -- then iterate until no Unknown left:
  --   if touching an In or the tip of an arrow
  --     set to In
  --   if touching an Out or back of an arrow
  --     set to Out
  -- then count number of In
  -- if it's not right, reverse U/D and L/R
  -- do
  --   start <- startLocation g
  --   moves <- validMoves g S
  --   let
  --     this = case moves of
  --       [ Dn, Up ] -> 𝖨
  --       [ Rt, Lf ] -> Ⲻ

  emptyGridMap g

_coord :: forall a. Coord -> AffineTraversal' NonEmptyArray a
_coord { x, y } = ix y >>> ix x

solve1 :: String -> Either ParseError Int
solve1 input =
  (_ / 2)
    <<< countStepsInLoop
    <$> spyWith "Pipes" show
    <$> runParser input parseGrid

countStepsInLoop :: Grid Pipe -> Int
countStepsInLoop g = fromMaybe 0 do
  start <- startLocation g
  moves <- validMoves g S
  first <- head moves
  pure $ go 0 start (opposite first)
  where
  go :: Int -> Coord -> Direction -> Int
  go steps c from =
    case get c g of
      Nothing -> steps
      Just pipe ->
        if pipe == S && steps > 0 then
          steps
        else
          case
            validMoves g pipe
              <#> filter (_ /= from)
              >>= head
            of
            Nothing -> steps
            Just d -> go (inc steps) (move c d) (opposite d)

startLocation :: Grid Pipe -> Maybe Coord
startLocation (Grid g) = lift2 { x: _, y: _ } x y
  where
  y = findIndex (S `elem` _) g
  x = elemIndex S =<< (g !! _) =<< y

parseGrid :: Parser String (Grid Pipe)
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

newtype Grid a = Grid (NonEmptyArray (NonEmptyArray a))

derive instance Eq a => Eq (Grid a)

instance Show a => Show (Grid a) where
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

derive instance Generic Direction _

instance Arbitrary Direction where
  arbitrary = genericArbitrary

instance Show Direction where
  show Up = "🠙"
  show Dn = "🠛"
  show Rt = "🠚"
  show Lf = "🠘"

validMoves :: Grid Pipe -> Pipe -> Maybe (Array Direction)
validMoves g = case _ of
  Г -> Just [ Rt, Dn ]
  𝖨 -> Just [ Dn, Up ]
  L -> Just [ Rt, Up ]
  𐐑 -> Just [ Dn, Lf ]
  𐐇 -> Just [ Lf, Up ]
  Ⲻ -> Just [ Rt, Lf ]
  O -> Nothing
  S -> startLocation g >>= \loc ->
    let
      valid = filter
        (pointsBackTo loc g)
        [ Up, Dn, Rt, Lf ]
    in
      case valid of
        arr@[ _, _ ] -> Just arr
        _ -> Nothing

pointsBackTo :: Coord -> Grid Pipe -> Direction -> Boolean
pointsBackTo c g d =
  get (move c d) g
    >>= validMoves g
    <#> map (_ == opposite d)
    # maybe false or

opposite :: Direction -> Direction
opposite Up = Dn
opposite Dn = Up
opposite Rt = Lf
opposite Lf = Rt

move :: Coord -> Direction -> Coord
move { x, y } = case _ of
  Up -> { x, y: y - 1 }
  Dn -> { x, y: y + 1 }
  Rt -> { x: x + 1, y }
  Lf -> { x: x - 1, y }

get :: forall a. Coord -> Grid a -> Maybe a
get { x, y } (Grid g) = (g !! y) >>= (\row -> row !! x)

data InOrOut
  = In
  | Out
  | EdgeUp
  | EdgeDown
  | EdgeLeft
  | EdgeRight
  | Corner
  | Unknown

derive instance Eq InOrOut

instance Show InOrOut where
  show In = "■"
  show Out = "▢"
  show EdgeUp = "⮝"
  show EdgeDown = "⮟"
  show EdgeLeft = "⮜"
  show EdgeRight = "⮞"
  show Corner = "⨯"
  show Unknown = "?"