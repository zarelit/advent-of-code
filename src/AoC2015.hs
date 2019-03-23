module AoC2015(
  challenges
) where

import Data.Maybe (fromJust)
import Data.List (elemIndex, sort)
import Challenge

year = 2015

challenges :: [(Issue, Challenge)]
challenges = [
  ((year, 1, "A"), Challenge santaFloor),
  ((year, 1, "B"), Challenge santaBasementIdx),
  ((year, 2, "A"), Challenge wrappingArea),
  ((year, 2, "B"), Challenge ribbonLength)
  ]

{-
--- Day 1: Not Quite Lisp ---

Santa was hoping for a white Christmas, but his weather machine's "snow"
function is powered by stars, and he's fresh out! To save Christmas, he needs
you to collect fifty stars by December 25th.

Collect stars by helping Santa solve puzzles. Two puzzles will be made available
on each day in the advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck!

Here's an easy puzzle to warm you up.

Santa is trying to deliver presents in a large apartment building, but he can't
find the right floor - the directions he got are a little confusing. He starts
on the ground floor (floor 0) and then follows the instructions one character at
a time.

An opening parenthesis, (, means he should go up one floor, and a closing
parenthesis, ), means he should go down one floor.

The apartment building is very tall, and the basement is very deep; he will
never find the top or bottom floors.

For example:

    (()) and ()() both result in floor 0. ((( and (()(()( both result in floor
    3. ))((((( also results in floor 3. ()) and ))( both result in floor -1 (the
    first basement level). ))) and )())()) both result in floor -3.

To what floor do the instructions take Santa?

-}

-- brackets are instruction to move, everything else can be a no op
move :: Char -> Int
move '(' = 1
move ')' = -1
move _ = 0

santaFloor :: String -> Integer
santaFloor = toInteger.sum.map move

{-
--- Part Two ---

Now, given the same instructions, find the position of the first character that
causes him to enter the basement (floor -1). The first character in the
instructions has position 1, the second character has position 2, and so on.

For example:

    ) causes him to enter the basement at character position 1. ()()) causes him
    to enter the basement at character position 5.

What is the position of the character that causes Santa to first enter the
basement?
-}

-- trace returns the list of floors Santa moves to when given some instructions
trace :: String -> [Int]
trace instructions = scanl (+) 0 (map move instructions)

santaBasementIdx :: String -> Integer
santaBasementIdx instructions = toInteger.fromJust $ elemIndex (-1) $ trace instructions

{-
--- Day 2: I Was Told There Would Be No Math ---

The elves are running low on wrapping paper, and so they need to submit an order
for more. They have a list of the dimensions (length l, width w, and height h)
of each present, and only want to order exactly as much as they need.

Fortunately, every present is a box (a perfect right rectangular prism), which
makes calculating the required wrapping paper for each gift a little easier:
find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves also
need a little extra paper for each present: the area of the smallest side.

For example:

    A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet
    of wrapping paper plus 6 square feet of slack, for a total of 58 square
    feet. A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42
    square feet of wrapping paper plus 1 square foot of slack, for a total of 43
    square feet.

All numbers in the elves' list are in feet. How many total square feet of
wrapping paper should they order?

-}

type Present = (Integer, Integer, Integer)

presentArea :: Present -> Integer
presentArea (l, w, h) = let
  smallestSides = take 2 $ sort [l, w, h]
  smallestSideArea = product smallestSides
  in smallestSideArea + 2*l*w + 2*w*h + 2*h*l

parsePresent :: String -> Present
parsePresent line = let
  spacesNoX = map (\x -> if x == 'x' then ' ' else x) line
  sidesString = words spacesNoX
  sidesNum = map read sidesString
  in (sidesNum !! 0, sidesNum !! 1, sidesNum !! 2)

presents :: String -> [Present]
presents input = map parsePresent $ lines input

wrappingArea :: String -> Integer
wrappingArea input = sum $ map presentArea (presents input)

{-
--- Part Two ---

The elves are also running low on ribbon. Ribbon is all the same width, so they
only have to worry about the length they need to order, which they would again
like to be exact.

The ribbon required to wrap a present is the shortest distance around its sides,
or the smallest perimeter of any one face. Each present also requires a bow made
out of ribbon as well; the feet of ribbon required for the perfect bow is equal
to the cubic feet of volume of the present. Don't ask how they tie the bow,
though; they'll never tell.

For example:''

    A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap
    the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34
    feet. A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon
    to wrap the present plus 1*1*10 = 10 feet of ribbon for the bow, for a total
    of 14 feet.

How many total feet of ribbon should they order?

-}

ribbonLength :: String -> Integer
ribbonLength input = sum $ map presentRibbon (presents input)

presentRibbon :: Present -> Integer
presentRibbon p = wrapRibbon p + bowRibbon p

-- smallest of all the perimeters
wrapRibbon :: Present -> Integer
wrapRibbon (l, w, h) = minimum [2*l + 2*w, 2*l + 2*h, 2*w + 2*h]

bowRibbon :: Present -> Integer
bowRibbon (l, w, h) = l * w * h
