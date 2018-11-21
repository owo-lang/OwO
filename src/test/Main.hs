module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Simple Test" $ do
    it "should work" $ do
      (2 + 2) `shouldBe` 4
