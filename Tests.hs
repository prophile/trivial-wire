module Main(main) where

import Prelude hiding ((.), id)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.FRP.Wire
import Control.FRP.Wire.Utils

import Control.Category
import Control.Arrow
import Control.Applicative

main :: IO ()
main = hspec $ do
  describe "Wire" $ do
    it "ignores its input for 'pure'" $ property $
      \x -> viewWire (pure "bees") [(x :: String)] == ["bees"]
    it "obeys identity" $ property $
      \x -> viewWire id x == (x :: String)
    it "obeys arr" $ property $
      \x f -> viewWire (arr (apply f)) (x :: [String]) ==
              (map (apply f) x :: [Int])

