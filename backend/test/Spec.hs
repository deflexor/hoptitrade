module Spec where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "HoptiTrade" $ do
    it "should be implemented" $ do
      True `shouldBe` True
