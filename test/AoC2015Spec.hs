module AoC2015Spec (spec) where

import DoesntHeHaveInternElvesForThis qualified
import IWasToldThereWouldBeNoMath qualified
import NotQuiteLisp qualified
import PerfectlySphericalHousesInAVacuum qualified
import ProbablyAFireHazard qualified
import TheIdealStockingStuffer qualified

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

        context "Day 02" $ do
            it "Part A" $ do
                let test = IWasToldThereWouldBeNoMath.partA
                test "2x3x4" `shouldBe` "58"
                test "1x1x10" `shouldBe` "43"
            it "Part B" $ do
                let test = IWasToldThereWouldBeNoMath.partB
                test "2x3x4" `shouldBe` "34"
                test "1x1x10" `shouldBe` "14"

        context "Day 03" $ do
            it "Part A" $ do
                let test = PerfectlySphericalHousesInAVacuum.partA
                test ">" `shouldBe` "2"
                test "^>v<" `shouldBe` "4"
                test "^v^v^v^v^v" `shouldBe` "2"
            it "Part B" $ do
                let test = PerfectlySphericalHousesInAVacuum.partB
                test "^v" `shouldBe` "3"
                test "^>v<" `shouldBe` "3"
                test "^v^v^v^v^v" `shouldBe` "11"

        context "Day 04" $ do
            it "Part A" $ do
                let test = TheIdealStockingStuffer.partA
                test "abcdef" `shouldBe` "609043"
                test "pqrstuv" `shouldBe` "1048970"

        context "Day 05" $ do
            it "Rule: Three Vowels" $ do
                let test = DoesntHeHaveInternElvesForThis.threeVowels
                test "aei" `shouldBe` True
                test "xazegov" `shouldBe` True
                test "aeiouaeiouaeiou" `shouldBe` True
                test "ugknbfddgicrmopn" `shouldBe` True
            it "Rule: Two in a row" $ do
                let test = DoesntHeHaveInternElvesForThis.twiceInARow
                test "xx" `shouldBe` True
                test "abcdde" `shouldBe` True
                test "aabbccdd" `shouldBe` True
                test "ugknbfddgicrmopn" `shouldBe` True
            it "Rule: No Naughty" $ do
                let test = DoesntHeHaveInternElvesForThis.noBlocklisted
                test "ugknbfddgicrmopn" `shouldBe` True
            it "Rule: Repeated pair no overlap" $ do
                let test = DoesntHeHaveInternElvesForThis.twicePairNoOverlap
                test "xyxy" `shouldBe` True
                test "aabcdefgaa" `shouldBe` True
                test "aaa" `shouldBe` False
            it "Rule: Repeated letter at distance one" $ do
                let test = DoesntHeHaveInternElvesForThis.repeatedLetterAtDistanceOne
                test "xyx" `shouldBe` True
                test "abcdefeghi" `shouldBe` True
                test "aaa" `shouldBe` True
            it "Part A" $ do
                let test = DoesntHeHaveInternElvesForThis.nice
                test "ugknbfddgicrmopn" `shouldBe` True
                test "aaa" `shouldBe` True
                test "jchzalrnumimnmhp" `shouldBe` False
                test "haegwjzuvuyypxyu" `shouldBe` False
                test "dvszwmarrgswjxmb" `shouldBe` False
            it "Part B" $ do
                let test = DoesntHeHaveInternElvesForThis.betterNice
                test "qjhvhtzxzqqjkmpb" `shouldBe` True
                test "xxyxx" `shouldBe` True
                test "uurcxstgmygtbstg" `shouldBe` False
                test "ieodomkazucvgmuy" `shouldBe` False

        context "Day 06" $ do
            it "Part A" $ do
                let test = ProbablyAFireHazard.partA
                test "turn on 0,0 through 999,999" `shouldBe` "1000000"
                test "toggle 0,0 through 999,0" `shouldBe` "1000"
                test
                    ( unlines
                        [ "turn on 0,0 through 1,0"
                        , "toggle 0,0 through 999,0"
                        ]
                    )
                    `shouldBe` "998"
                test
                    ( unlines
                        [ "turn on 0,0 through 999,999"
                        , "turn off 499,499 through 500,500"
                        ]
                    )
                    `shouldBe` "999996"
            it "Part B" $ do
                let test = ProbablyAFireHazard.partB
                test "turn on 0,0 through 999,999" `shouldBe` "1000000"
                test "toggle 0,0 through 999,0" `shouldBe` "2000"
                test
                    ( unlines
                        [ "turn on 0,0 through 1,0"
                        , "toggle 0,0 through 999,0"
                        ]
                    )
                    `shouldBe` "2002"
                test
                    ( unlines
                        [ "turn on 0,0 through 999,999"
                        , "turn off 499,499 through 500,500"
                        , "turn on 499,499 through 499,499"
                        ]
                    )
                    `shouldBe` "999997"
        context "Day 07" $ do
            it "Part A" $ do
                let test = SomeAssemblyRequired.partA
