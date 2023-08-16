module PerfectlySphericalHousesInAVacuum where

import Common (parse, toEof)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (nub)
import Text.ParserCombinators.ReadP (ReadP, char, choice, many1)

-- --- Day 3: Perfectly Spherical Houses in a Vacuum ---

-- Santa is delivering presents to an infinite two-dimensional grid of houses.

-- He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him via radio and tells him where to move next. Moves are always exactly one house to the north (^), south (v), east (>), or west (<). After each move, he delivers another present to the house at his new location.

-- However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and Santa ends up visiting some houses more than once. How many houses receive at least one present?

-- For example:

--     > delivers presents to 2 houses: one at the starting location, and one to the east.
--     ^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
--     ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.

data Instruction = North | East | South | West deriving (Show)

instruction :: ReadP Instruction
instruction =
    choice
        [ North <$ char '^'
        , East <$ char '>'
        , West <$ char '<'
        , South <$ char 'v'
        ]

instructionsForOne :: ReadP [Instruction]
instructionsForOne = toEof (many1 instruction)

-- Coord = (north, east)
type Coord = (Int, Int)

move :: Instruction -> Coord -> Coord
move North (n, e) = (n + 1, e)
move East (n, e) = (n, e + 1)
move South (n, e) = (n - 1, e)
move West (n, e) = (n, e - 1)

origin :: Coord
origin = (0, 0)

path :: [Instruction] -> [Coord]
path = scanl (flip move) origin

visitedHouses :: [Coord] -> Int
visitedHouses = length . nub

partA :: String -> String
partA = show . visitedHouses . path . parse instructionsForOne

-- --- Part Two ---

-- The next year, to speed up the process, Santa creates a robot version of himself, Robo-Santa, to deliver presents with him.

-- Santa and Robo-Santa start at the same location (delivering two presents to the same starting house), then take turns moving based on instructions from the elf, who is eggnoggedly reading from the same script as the previous year.

-- This year, how many houses receive at least one present?

-- For example:

--     ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
--     ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
--     ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.

-- uninterleave will split the list into two based on odd or even positions
uninterleave :: [a] -> ([a], [a])
uninterleave [] = ([], [])
uninterleave [x] = ([x], [])
uninterleave (a : b : xs) = (a : as, b : bs) where (as, bs) = uninterleave xs

instructionsForTwo :: ReadP ([Instruction], [Instruction])
instructionsForTwo = uninterleave <$> instructionsForOne

partB :: String -> String
partB =
    show . visitedHouses . uncurry (++) . bimap path path . parse instructionsForTwo
