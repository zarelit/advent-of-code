-- module AoC2015.Day06 (
--   partA, partB
-- )
module AoC2015.Day06 (partA, partB)
where
import Challenge

import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha)
import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (mapMaybe)

{-
--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating
contest year after yearundefined, you've decided to deploy one million lights in a
1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you
instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at
each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include
whether to turn on, turn off, or toggle various inclusive ranges given as
coordinate pairs. Each coordinate pair represents opposite corners of a
rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to
9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by
doing the instructions Santa sent you in order.

For example:

    turn on 0,0 through 999,999 would turn on (or leave on) every light. toggle
    0,0 through 999,0 would toggle the first line of 1000 lights, turning off
    the ones that were on, and turning on the ones that were off. turn off
    499,499 through 500,500 would turn off (or leave off) the middle four
    lights.

After following the instructions, how many lights are lit?
-}

data Instruction = Instruction Op Range deriving Show
data Op = Off | On | Toggle deriving Show
data Range = Range Start End deriving Show
type Start = Point
type End = Start
data Point = Point Int Int deriving Show

instruction :: ReadP Instruction
instruction = do
  op <- operation
  char ' '
  r <- range
  return $ Instruction op r

operation :: ReadP Op
operation = let
  -- parse any string with spaces and letters and let opLookup be the source
  -- of truth for the String reprentation of Op
  opStr = many1 (satisfy isAlpha <|> satisfy (== ' '))
  in do
    op <- opStr
    maybe pfail return $ opLookup op

opLookup :: String -> Maybe Op
opLookup "turn on" = Just On
opLookup "turn off" = Just Off
opLookup "toggle" = Just Toggle
opLookup _ = Nothing

range :: ReadP Range
range = do
  a <- point
  string " through "
  b <- point
  return $ Range a b

point :: ReadP Point
point = let
  number = fmap read (many1 $ satisfy isDigit)
  in do
    a <- number
    satisfy (== ',')
    b <- number
    return $ Point a b

parseInstruction :: String -> Maybe Instruction
parseInstruction s = let
  isComplete (_, rest) = rest == ""
  in fst <$> find isComplete (readP_to_S instruction s)

parse :: String -> [Instruction]
parse = mapMaybe parseInstruction . lines


partA = undefined

partB = undefined
