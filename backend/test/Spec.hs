module Main where

import Domain.Kelly
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Kelly" $ do
    it "even money p=0.6 b=1 is 0.2" $
      kellyFraction 0.6 1 `shouldBe` 0.2
    it "even money p=0.4 b=1 is 0" $
      kellyFraction 0.4 1 `shouldBe` 0
    it "size floors to 0 when stake < maxLoss" $
      kellyQuantity 0.6 10 10000 0.5 1000 2 `shouldBe` 0
