module AoC2015Spec (spec) where

import Test.Hspec
import ChallengeLauncher

spec :: Spec
spec =
  describe "AoC2015" $ do
    let year = 2015
    context "Day 01" $ do
      let day = 1
      it "Part A" $ do
        let test = run (year, day, "A")
        test "(())" `shouldBe` 0
        test "()()" `shouldBe` 0
        test "(((" `shouldBe` 3
        test "(()(()(" `shouldBe` 3
        test "))(((((" `shouldBe` 3
        test "())" `shouldBe` -1
        test "))(" `shouldBe` -1
        test ")))" `shouldBe` -3
        test ")())())" `shouldBe` -3
      it "Part B" $ do
        let test = run (year, day, "B")
        test ")" `shouldBe` 1
        test "()())" `shouldBe` 5

    context "Day 02" $ do
      let day = 2
      it "Part A" $ do
        let test = run (year, day, "A")
        test "2x3x4" `shouldBe` 58
        test "1x1x10" `shouldBe` 43
      it "Part B" $ do
        let test = run (year, day, "B")
        test "2x3x4" `shouldBe` 34
        test "1x1x10" `shouldBe` 14

    context "Day 03" $ do
      let day = 3
      it "Part A" $ do
        let test = run (year, day, "A")
        test ">" `shouldBe` 2
        test "^>v<" `shouldBe` 4
        test "^v^v^v^v^v" `shouldBe` 2
      it "Part B" $ do
        let test = run (year, day, "B")
        test "^v" `shouldBe` 3
        test "^>v<" `shouldBe` 3
        test "^v^v^v^v^v" `shouldBe` 11

    context "Day 04" $ do
      let day = 4
      it "Part A" $ do
        let test = run (year, day, "A")
        test "abcdef" `shouldBe` 609043
        test "pqrstuv" `shouldBe` 1048970
