{-# LANGUAGE OverloadedStrings #-}

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
import Control.Arrow.Operations(delay)
import Control.Arrow.Signal

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
    it "can form a circuit" $ property $
      \x y z -> viewWire (delay 0) [x, y, z] == [0 :: Int, x, y]

  describe "the utilities" $ do
    it "can differentiate streams of integers" $ property $ do
      \x y z -> viewWire (differentiate 0 (-)) [x, y, z] ==
              [x :: Integer, y - x, z - y]
    it "can integrate streams of integers" $ property $ do
      \x y z -> viewWire wsum [x, y, z] ==
              [x :: Integer, x + y, x + y + z]

  describe "signal combinators" $ do
    it "can latch signals" $ property $ do
      \x y -> viewWire wlatch [Nothing, Just x, Nothing, Just y] `shouldBe`
                              [Nothing, Just (x :: Int), Just x, Just y]
    it "can wait for initials from signals" $ property $ do
      \x y -> viewWire wdecisive [Nothing, Just x, Nothing, Just y] `shouldBe`
                                 [Nothing, Just (x :: Int), Just x, Just x]
    it "can detect and latch changes" $ property $ do
      \x -> viewWire (changes >>> wlatch) x `shouldBe` map Just (x :: [Int])

  describe "the convenience instances" $ do
    it "can handle strings" $ do
      viewWire "badgers" [undefined] `shouldBe` ["badgers"]

  describe "liftSignal" $ do
    it "acts only on events" $ property $ do
      \x -> viewWire (liftSignal $ arr (* (2 :: Int)) >>> arr Just) x `shouldBe`
            map (fmap (* 2)) x

