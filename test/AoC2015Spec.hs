module AoC2015Spec (spec) where

import NotQuiteLisp qualified
import Test.Hspec

spec :: Spec
spec =
    describe "AoC2015" $ do
        context "Day 01" $ do
            it "Part A" $ do
                let test = NotQuiteLisp.santasFloor
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
                let test = NotQuiteLisp.santaBasementIdx
                test ")" `shouldBe` 1
                test "()())" `shouldBe` 5

-- context "Day 02" $ do
--   let day = 2
--   it "Part A" $ do
--     let test = run (year, day, "A")
--     test "2x3x4" `shouldBe` 58
--     test "1x1x10" `shouldBe` 43
--   it "Part B" $ do
--     let test = run (year, day, "B")
--     test "2x3x4" `shouldBe` 34
--     test "1x1x10" `shouldBe` 14

-- context "Day 03" $ do
--   let day = 3
--   it "Part A" $ do
--     let test = run (year, day, "A")
--     test ">" `shouldBe` 2
--     test "^>v<" `shouldBe` 4
--     test "^v^v^v^v^v" `shouldBe` 2
--   it "Part B" $ do
--     let test = run (year, day, "B")
--     test "^v" `shouldBe` 3
--     test "^>v<" `shouldBe` 3
--     test "^v^v^v^v^v" `shouldBe` 11

-- context "Day 04" $ do
--   let day = 4
--   it "Part A" $ do
--     let test = run (year, day, "A")
--     test "abcdef" `shouldBe` 609043
--     test "pqrstuv" `shouldBe` 1048970

-- context "Day 05" $ do
--   let day = 5
--   it "Part A" $ do
--     let test = run (year, day, "A")
--     test "ugknbfddgicrmopn" `shouldBe` 1
--     test "aaa" `shouldBe` 1
--     test "jchzalrnumimnmhp" `shouldBe` 0
--     test "haegwjzuvuyypxyu" `shouldBe` 0
--     test "dvszwmarrgswjxmb" `shouldBe` 0
--   it "Part B" $ do
--     let test = run (year, day, "B")
--     test "qjhvhtzxzqqjkmpb" `shouldBe` 1
--     test "xxyxx" `shouldBe` 1
--     test "uurcxstgmygtbstg" `shouldBe` 0
--     test "ieodomkazucvgmuy" `shouldBe` 0

-- context "Day 06" $ do
--   let day = 6
--   it "Part A" $ do
--     let test = run (year, day, "A")
--     test "turn on 0,0 through 999,999" `shouldBe` 1000000
--     test "toggle 0,0 through 999,0" `shouldBe` 1000
--     test (unlines [
--       "turn on 0,0 through 1,0",
--       "toggle 0,0 through 999,0"
--       ]) `shouldBe` 998
--     test (unlines [
--       "turn on 0,0 through 999,999",
--       "turn off 499,499 through 500,500"
--       ]) `shouldBe` 999996
