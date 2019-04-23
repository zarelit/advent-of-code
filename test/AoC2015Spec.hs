module AoC2015Spec (spec) where

import Test.Hspec
import ChallengeLauncher

spec :: Spec
spec = do
  describe "AoC2015" $ do
    context "Day 01" $ do
      it "Part A" $ do
        run (2015, 1, "A") "(())" `shouldBe` 0
        run (2015, 1, "A") "()()" `shouldBe` 0
