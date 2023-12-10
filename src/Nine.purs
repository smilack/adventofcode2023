module AdventOfCode.Twenty23.Nine
  ( derivative
  , findConstFn
  , main
  , next
  , parseHistories
  , solve1
  , solve2
  ) where

import Prelude

import Data.Either (Either)
import Data.Foldable (all)
import Data.List.NonEmpty (NonEmptyList, foldMap, foldr, fromList, last, reverse, scanl, singleton, uncons)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr1)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (sepBy1, sepEndBy1)
import Parsing.String (char)
import Parsing.String.Basic (intDecimal)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/9"
  liftEffect do
    log "Part 1:"
    log "sum of predicted next values"
    logShow $ solve1 input
    log "Part2:"
    log "sum of predicted prev values"
    logShow $ solve2 input

solve1 :: String -> Either ParseError Int
solve1 input = do
  histories <- runParser input parseHistories
  pure
    $ unwrap
    $ foldMap
        (Additive <<< next <<< findConstFn)
        histories

solve2 :: String -> Either ParseError Int
solve2 input = do
  histories <- runParser input parseHistories
  pure
    $ unwrap
    $ foldMap
        (Additive <<< next <<< findConstFn <<< reverse)
        histories

parseHistories :: Parser String (NonEmptyList (NonEmptyList Int))
parseHistories = parseHistory `sepEndBy1` char '\n'

parseHistory :: Parser String (NonEmptyList Int)
parseHistory = intDecimal `sepBy1` char ' '

next :: forall a. Ring a => NonEmptyList (NonEmptyList a) -> a
next = foldr (\l a -> last l + a) zero

findConstFn
  :: forall a
   . Eq a
  => Ring a
  => NonEmptyList a
  -> NonEmptyList (NonEmptyList a)
findConstFn = unfoldr1 go
  where
  go f =
    let
      f' =
        if all (_ == zero) f then
          Nothing
        else
          Just $ derivative f
    in
      Tuple f f'

derivative :: forall a. Ring a => NonEmptyList a -> NonEmptyList a
derivative = map (\(Tuple a b) -> b - a) <<< pairs

pairs :: forall a. NonEmptyList a -> NonEmptyList (Tuple a a)
pairs = uncons >>> \{ head, tail } ->
  case fromList tail of
    Nothing -> singleton $ Tuple head head
    Just t -> scanl pair (Tuple head head) t
  where
  pair (Tuple _ pre) nex = Tuple pre nex