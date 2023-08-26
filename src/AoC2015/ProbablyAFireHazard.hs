{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module ProbablyAFireHazard where

import Common (parseToEof, unsignedNumber)
import Text.ParserCombinators.ReadP (ReadP, char, choice, many1, pfail, skipSpaces, string)

-- --- Day 6: Probably a Fire Hazard ---

-- Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.

-- Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.

-- Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

-- To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

-- For example:

--     turn on 0,0 through 999,999 would turn on (or leave on) every light.
--     toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
--     turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

{- Parse Input -}
data Op = TurnOn | TurnOff | Toggle
newtype Instruction = Instruction (Op, Rect)
data Rect = Rect Point Point
data Point = Point Int Int

-- parse a single point, fail if coordinates are out of range
point :: ReadP Point
point = do
    x <- unsignedNumber
    _ <- char ','
    y <- unsignedNumber
    if x < 0 || x > 999 || y < 0 || y > 999
        then pfail
        else return $ Point x y

-- parse a rectangle, fail if first point is not the lower-left one
rect :: ReadP Rect
rect = do
    (Point a b) <- point
    _ <- string " through "
    (Point c d) <- point
    if a > c || b > d
        then pfail
        else return $ Rect (Point a b) (Point c d)

opcode :: ReadP Op
opcode =
    choice
        [ TurnOn <$ string "turn on"
        , TurnOff <$ string "turn off"
        , Toggle <$ string "toggle"
        ]

instruction :: ReadP Instruction
instruction = do
    op <- opcode
    _ <- skipSpaces
    r <- rect
    _ <- skipSpaces
    return $ Instruction (op, r)

{-
    In my first attempt of a solution of this exercise in Haskell I chose to represent
    data on a grid. I tried with several libraries but it ended with laziness issues:
    https://stackoverflow.com/questions/56507414/debug-memory-issue-in-haskell
    https://github.com/zarelit/advent-of-code/blob/6bae8c2a5f907cf0ae55dd1fdd1ab6f5d6c73789/src/AoC2015/Day06/Calculate.hs

    There's no need to keep all the points in memory, every point is calculated on its own so the new approach is:
    - create a list of 1 million coordinates
    - filter instructions that apply to them
    - apply them
-}

-- All the points
points :: [(Int, Int)]
points = [(x, y) | x <- [0 .. 999], y <- [0 .. 999]]

-- Check if instruction applies to point
mustBeApplied :: (Int, Int) -> Instruction -> Bool
mustBeApplied (x, y) (Instruction (_, Rect (Point a b) (Point c d))) = x >= a && x <= c && y >= b && y <= d

instructionsForPoint :: (Int, Int) -> [Instruction] -> [Instruction]
instructionsForPoint p = filter (mustBeApplied p)

-- Actual work on the lights
stepA :: Instruction -> Bool -> Bool
stepA (Instruction (TurnOn, _)) = const True
stepA (Instruction (TurnOff, _)) = const False
stepA (Instruction (Toggle, _)) = not

-- calculate one single light applying all the instructions
pixelA :: [Instruction] -> Bool
pixelA = foldl (flip stepA) False

-- Applies the instructions to all the points
pictureA :: [Instruction] -> [Bool]
pictureA fs = map (\p -> pixelA $ instructionsForPoint p fs) points

partA :: String -> String
partA = show . length . filter id . pictureA . parseToEof (many1 instruction)

-- --- Part Two ---

-- You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.

-- The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.

-- The phrase turn on actually means that you should increase the brightness of those lights by 1.

-- The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.

-- The phrase toggle actually means that you should increase the brightness of those lights by 2.

-- What is the total brightness of all lights combined after following Santa's instructions?

-- For example:

--     turn on 0,0 through 0,0 would increase the total brightness by 1.
--     toggle 0,0 through 999,999 would increase the total brightness by 2000000.

stepB :: Instruction -> Int -> Int
stepB (Instruction (TurnOn, _)) current = current + 1
stepB (Instruction (TurnOff, _)) current = max 0 (current - 1)
stepB (Instruction (Toggle, _)) current = current + 2

pixelB :: [Instruction] -> Int
pixelB = foldl (flip stepB) 0

pictureB :: [Instruction] -> [Int]
pictureB fs = map (\p -> pixelB $ instructionsForPoint p fs) points

brightness :: [Instruction] -> Int
brightness = sum . pictureB

partB :: String -> String
partB = show . brightness . parseToEof (many1 instruction)
