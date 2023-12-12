module AdventOfCode.Twenty23.Ten
  ( Direction(..)
  , Grid(..)
  , InOrOut(..)
  , Pipe(..)
  , countStepsInLoop
  , emptyGridMap
  , main
  , mapPipeEdges
  , move
  , opposite
  , parseGrid
  , parsePipe
  , pointsBackTo
  , solve1
  , solve2
  , startLocation
  , validMoves
  ) where

import AdventOfCode.Twenty23.Util (inc, sumMap)
import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array (any, catMaybes, filter, head)
import Data.Array.NonEmpty (NonEmptyArray, elem, elemIndex, findIndex, foldMap1, fromFoldable1, intercalate, length, mapWithIndex, replicate, uncons, (!!))
import Data.Either (Either)
import Data.Foldable (or)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Traversal', anyOf, iso, preview, toArrayOf, traversed)
import Data.Lens (set) as Lens
import Data.Lens.Index (class Index, ix)
import Data.Lens.Types (AffineTraversal')
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (sepEndBy1, (<?>))
import Parsing.Combinators.Array (many1)
import Parsing.String (char)
import Parsing.String.Basic (skipSpaces)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/10"
  liftEffect do
    log "Part 1:"
    log "Half loop length"
    logShow $ solve1 input
    log "Part2:"
    log "Number of in/out tiles:"
    logShow $ solve2 input

-- Each loop of pipes will always have an even
-- number of segments. Therefore the number of
-- steps to the farthest segment from the start
-- is half the length of the loop.

type Coord = { x :: Int, y :: Int }

solve2 :: String -> Either ParseError { in :: Int, out :: Int }
solve2 input =
  go
    <$> toArrayOf _every
    <$> mapPipeEdges
    -- <$> spyWith "Pipes" show
    <$> runParser input parseGrid
  where
  go :: Array InOrOut -> { in :: Int, out :: Int }
  go arr =
    { in: sumMap ((_ == In) >>> if _ then 1 else 0) arr
    , out: sumMap ((_ == Out) >>> if _ then 1 else 0) arr
    }

emptyGridMap :: Grid Pipe -> Grid InOrOut
emptyGridMap (Grid nea) = Grid $
  replicate (length nea)
    (replicate (length head) Unknown)
  where
  { head } = uncons nea

mapPipeEdges :: Grid Pipe -> Grid InOrOut
mapPipeEdges g = fromMaybe (emptyGridMap g)
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
  do
    start <- startLocation g
    moves <- validMoves g S
    from <- head moves
    let
      this = case moves of
        [ Rt, Dn ] -> Г
        [ Dn, Rt ] -> Г
        [ Dn, Up ] -> 𝖨
        [ Up, Dn ] -> 𝖨
        [ Rt, Up ] -> L
        [ Up, Rt ] -> L
        [ Dn, Lf ] -> 𐐑
        [ Lf, Dn ] -> 𐐑
        [ Lf, Up ] -> 𐐇
        [ Up, Lf ] -> 𐐇
        [ Rt, Lf ] -> Ⲻ
        [ Lf, Rt ] -> Ⲻ
        _ -> O
      g' = set start this g
      edgeMap = findEdges start from g' (emptyGridMap g')
    pure $ analyzePath edgeMap

findEdges :: Coord -> Direction -> Grid Pipe -> Grid InOrOut -> Grid InOrOut
findEdges loc from pipes edges =
  case get loc edges of
    Nothing -> edges
    Just Unknown ->
      case get loc pipes of
        Nothing -> edges
        Just pipe ->
          let
            edge' = case pipe of
              Г -> if from == Dn then CornerDownRight else CornerUpLeft
              𝖨 -> if from == Up then EdgeLeft else EdgeRight
              L -> if from == Rt then CornerUpRight else CornerDownLeft
              𐐑 -> if from == Lf then CornerDownLeft else CornerUpRight
              𐐇 -> if from == Up then CornerUpLeft else CornerDownRight
              Ⲻ -> if from == Lf then EdgeDown else EdgeUp
              O -> Unknown
              S -> Unknown
            edges' = set loc edge' edges
          in
            case
              validMoves pipes pipe
                <#> filter (_ /= from)
                >>= head
              of
              Nothing -> edges'
              Just next ->
                findEdges (move loc next) (opposite next) pipes edges'
    _ -> edges

analyzePath :: Grid InOrOut -> Grid InOrOut
analyzePath gr =
  if anyOf _every (_ == Unknown) gr then
    analyzePath (iteration gr)
  else
    gr
  where
  iteration (Grid g) = Grid (mapWithIndex updateRow g)
  updateRow y r = mapWithIndex (\x a -> update { x, y } a) r
  update c a =
    if a == Unknown then
      let
        nu = neighbor c gr Up
        nd = neighbor c gr Dn
        nl = neighbor c gr Lf
        nr = neighbor c gr Rt
        neighbors = catMaybes [ nu, nd, nl, nr ]
      in
        if
          nu == Just EdgeDown
            || nu == Just CornerDownLeft
            || nu == Just CornerDownRight
            || nd == Just EdgeUp
            || nd == Just CornerUpLeft
            || nd == Just CornerUpRight
            || nl == Just EdgeRight
            || nl == Just CornerUpRight
            || nl == Just CornerDownRight
            || nr == Just EdgeLeft
            || nr == Just CornerUpLeft
            || nr == Just CornerDownLeft
            || any (_ == In) neighbors then
          In
        else if
          nu == Just EdgeUp
            || nu == Just CornerUpLeft
            || nu == Just CornerUpRight
            || nd == Just EdgeDown
            || nd == Just CornerDownLeft
            || nd == Just CornerDownRight
            || nl == Just EdgeLeft
            || nl == Just CornerUpLeft
            || nl == Just CornerDownLeft
            || nr == Just EdgeRight
            || nr == Just CornerUpRight
            || nr == Just CornerDownRight
            || any (_ == Out) neighbors then
          Out
        else
          a
    else
      a

solve1 :: String -> Either ParseError Int
solve1 input =
  (_ / 2)
    <<< countStepsInLoop
    -- <$> spyWith "Pipes" show
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

derive instance Newtype (Grid a) _

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

_coord
  :: forall m1 m2 b
   . Index m1 Int b
  => Index m2 Int m1
  => Coord
  -> AffineTraversal' m2 b
_coord { x, y } = ix y <<< ix x

_grid :: forall a. Iso' (Grid a) (NonEmptyArray (NonEmptyArray a))
_grid = iso unwrap wrap

_cell :: forall a. Coord -> AffineTraversal' (Grid a) a
_cell c = _grid <<< (_coord c)

neighbor :: forall a. Coord -> Grid a -> Direction -> Maybe a
neighbor c g d = get (move c d) g

get :: forall a. Coord -> Grid a -> Maybe a
get c = preview (_cell c)

set :: forall a. Coord -> a -> Grid a -> Grid a
set c = Lens.set (_cell c)

_every :: forall a. Traversal' (Grid a) a
_every = _grid <<< traversed <<< traversed

data InOrOut
  = In
  | Out
  | EdgeUp
  | EdgeDown
  | EdgeLeft
  | EdgeRight
  | CornerUpRight
  | CornerUpLeft
  | CornerDownRight
  | CornerDownLeft
  | Unknown

derive instance Eq InOrOut

instance Show InOrOut where
  show In = "■"
  show Out = "▢"
  show EdgeUp = "⮝"
  show EdgeDown = "⮟"
  show EdgeLeft = "⮜"
  show EdgeRight = "⮞"
  show CornerUpRight = "🡽"
  show CornerUpLeft = "🡼"
  show CornerDownRight = "🡾"
  show CornerDownLeft = "🡿"
  show Unknown = "?"