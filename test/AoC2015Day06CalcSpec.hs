module AoC2015Day06CalcSpec (spec) where

import Test.Hspec
import AoC2015.Day06.Calculate
import AoC2015.Day06.Types

spec :: Spec
spec =
  describe "Day06 calculations" $ do
    context "toLinear" $ do
      let test = toLinear
      it "maps the origin" $ test (Point 0 0) `shouldBe` 0
      it "maps point on first row" $ test (Point 0 5) `shouldBe` 5
      it "maps point on different row" $ test (Point 2 2) `shouldBe` 2002
    context "fromLinear" $ do
      let test = fromLinear
      it "maps the origin" $ test 0 `shouldBe` Point 0 0
      it "maps point on first row" $ test 5 `shouldBe` Point 0 5
      it "maps point on different row" $ test 2002 `shouldBe` Point 2 2
