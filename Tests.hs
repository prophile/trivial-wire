module Main(main) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "the test suite" $ do
    it "passes this trivial example" $
      property $ \x -> (x + 1) > (x :: Integer)
