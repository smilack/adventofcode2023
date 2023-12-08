module Test.AdventOfCode.Twenty23.Util
  ( Foo(..)
  , main
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (isLeft)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Parsing (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Util" do
    describe "dec" do
      it "dec ints" do
        dec 100 `shouldEqual` 99
        dec 2 `shouldEqual` 1
        dec 1 `shouldEqual` 0
        dec 0 `shouldEqual` (-1)
        dec (-100) `shouldEqual` (-101)
      it "dec numbers" do
        dec 100.0 `shouldEqual` 99.0
        dec 2.0 `shouldEqual` 1.0
        dec 1.0 `shouldEqual` 0.0
        dec 0.0 `shouldEqual` (-1.0)
        dec (-100.0) `shouldEqual` (-101.0)
    describe "oneOf'" do
      let
        foosS = [ Tuple "A" A, Tuple "B" B, Tuple "C" C ]
        foosC = [ Tuple 'A' A, Tuple 'B' B, Tuple 'C' C ]
      it "oneOfChar success" do
        testParser "A" A $ oneOfChar foosC
        testParser "B" B $ oneOfChar foosC
        testParser "C" C $ oneOfChar foosC
      it "oneOfChar error" do
        (runParser "D" $ oneOfChar foosC) `shouldSatisfy` isLeft
      it "oneOfString" do
        testParser "A" A $ oneOfString foosS
        testParser "B" B $ oneOfString foosS
        testParser "C" C $ oneOfString foosS
      it "oneOfString error" $ do
        (runParser "D" $ oneOfString foosS) `shouldSatisfy` isLeft
      it "genericParser" $ do
        testParser "A" A $ genericParser @Foo
        testParser "B" B $ genericParser @Foo
        testParser "C" C $ genericParser @Foo
      it "genericParser error" $ do
        (runParser "D" $ genericParser @Foo) `shouldSatisfy` isLeft

data Foo = A | B | C

derive instance Eq Foo
derive instance Ord Foo
derive instance Generic Foo _
instance Show Foo where
  show = genericShow

instance Enum Foo where
  succ = genericSucc
  pred = genericPred

instance Bounded Foo where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Foo where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum