module Test.AdventOfCode.Twenty23.Util
  ( main
  ) where

import AdventOfCode.Twenty23.Util
import Prelude

import Data.Either (isLeft)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Parsing (runParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Util" do
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

data Foo = A | B | C

derive instance Eq Foo
derive instance Generic Foo _
instance Show Foo where
  show = genericShow