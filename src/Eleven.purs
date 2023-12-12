module AdventOfCode.Twenty23.Eleven
  ( Coord
  , Image(..)
  , Space(..)
  , _cell
  , _col
  , _row
  , expand
  , findEmpty
  , findGalaxies
  , getShortestPaths
  , isEmpty
  , main
  , parseImage
  , solve1
  , solve2
  ) where

import AdventOfCode.Twenty23.Util (inc, sumMap)
import Prelude

import Control.Alt ((<|>))
import Data.Array (foldMap, foldr, fromFoldable, insertAt, intercalate, snoc, (!!))
import Data.Either (Either)
import Data.Enum (fromEnum)
import Data.Foldable (sum)
import Data.Lens (Iso', Traversal', allOf, iso, lengthOf, preview, traversed)
import Data.Lens.Index (ix)
import Data.Lens.Types (AffineTraversal')
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import JS.BigInt (BigInt, fromInt)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (sepEndBy, (<?>))
import Parsing.Combinators.Array (many)
import Parsing.String (char)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/11"
  liftEffect do
    log "Part 1:"
    log "Sum of shortest paths between galaxies"
    logShow $ solve1 input
    log "Part2:"
    log "Sum of shortest paths between galaxies far far away"
    logShow $ solve2 input

solve2 :: String -> Either ParseError BigInt
solve2 input = runParser input parseImage <#> \image ->
  let
    rows = findEmpty _row image
    cols = findEmpty _col image
    galaxies = findGalaxies image
  in
    getDistsWithGaps galaxies rows cols

getDistsWithGaps :: Array Coord -> Array Int -> Array Int -> BigInt
getDistsWithGaps galaxies rows cols =
  (_ / fromInt 2) $ sum do
    c1 <- galaxies
    c2 <- galaxies
    let
      baseDist = dist c1 c2
      numGaps = nBetween c1.y c2.y rows + nBetween c1.x c2.x cols
    pure $ fromInt $ baseDist + 999_999 * numGaps

nBetween :: Int -> Int -> Array Int -> Int
nBetween a b = sumMap (fromEnum <<< bet)
  where
  bet = between (min a b) (max a b)

solve1 :: String -> Either ParseError Int
solve1 input =
  getShortestPaths
    <$> findGalaxies
    <$> expand
    <$> runParser input parseImage

data Space = Galaxy | Empty

derive instance Eq Space

instance Show Space where
  show Galaxy = "#"
  show Empty = "."

newtype Image = Image (Array (Array Space))

derive instance Newtype Image _

instance Show Image where
  show (Image i) = intercalate "\n" $ foldMap show <$> i

parseImage :: Parser String Image
parseImage = wrap <$> fromFoldable <$> parseLine `sepEndBy` char '\n'
  where
  parseLine = many do
    Empty <$ char '.'
      <|> Galaxy <$ char '#'
        <?> "Space (# or .)"

isEmpty :: Traversal' Image Space -> Image -> Boolean
isEmpty t i =
  lengthOf t i > 0
    && allOf t (_ == Empty) i

_image :: Iso' Image (Array (Array Space))
_image = iso unwrap wrap

_col :: Int -> Traversal' Image Space
_col i = _image <<< traversed <<< ix i

_row :: Int -> Traversal' Image Space
_row i = _image <<< ix i <<< traversed

_cell :: Coord -> AffineTraversal' Image Space
_cell { x, y } = _image <<< ix y <<< ix x

isGalaxy :: Coord -> Image -> Boolean
isGalaxy c i = preview (_cell c) i == Just Galaxy

findEmpty :: (Int -> Traversal' Image Space) -> Image -> Array Int
findEmpty t i = go 0 []
  where
  go x found
    | lengthOf (t x) i <= 0 = found
    | allOf (t x) (_ == Empty) i = go (inc x) (snoc found x)
    | otherwise = go (inc x) found

expand :: Image -> Image
expand i@(Image arr) = Image $
  foldr copy (foldr (map <<< copy) arr cols) rows
  where
  rows = findEmpty _row i
  cols = findEmpty _col i

copy :: forall a. Int -> Array a -> Array a
copy x a = fromMaybe a do
  r <- a !! x
  insertAt x r a

type Coord = { x :: Int, y :: Int }

findGalaxies :: Image -> Array Coord
findGalaxies i = go { x: 0, y: 0 } []
  where
  go c@{ x, y } found
    | lengthOf (_row y) i == 0 = found
    | x >= lengthOf (_row y) i = go { x: 0, y: inc y } found
    | isGalaxy c i = go { x: inc x, y } (snoc found c)
    | otherwise = go { x: inc x, y } found

getShortestPaths :: Array Coord -> Int
getShortestPaths arr = (_ / 2) $ sum do
  c1 <- arr
  c2 <- arr
  pure $ dist c1 c2

dist :: Coord -> Coord -> Int
dist c1 c2 = abs (c1.x - c2.x) + abs (c1.y - c2.y)