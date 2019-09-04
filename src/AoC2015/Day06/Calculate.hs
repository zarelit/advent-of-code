module AoC2015.Day06.Calculate (
runChallenge
) where

import AoC2015.Day06.Types
import Numeric.Matrix

-- as per the exercise definition
width = 1000
height = 1000

runChallenge :: [Instruction] -> Integer
runChallenge xs = (toInteger.Numeric.Matrix.sum) (endMatrix xs)

startMatrix :: Matrix Int
startMatrix = matrix (width, height) (const 0)

endMatrix :: [Instruction] -> Matrix Int
endMatrix = foldl step startMatrix

contains :: Range -> (Int, Int) -> Bool
contains (Range start end) p = let
  Point x1 y1 = start
  Point x2 y2 = end
  (xp, yp) = p
  (mxp, myp) = (xp-1, yp-1)
  in
    mxp <= max x1 x2 && mxp >= min x1 x2 &&
    myp <= max y1 y2 && myp >= min y1 y2

step :: Matrix Int -> Instruction -> Matrix Int
step z i = mapWithIndex (apply i) z

-- apply the instruction on a single item in the matrix
apply :: Instruction -> (Int, Int) -> Int -> Int
apply (Instruction On r) p x = applyIfInRange r p (const 1) x
apply (Instruction Off r) p x = applyIfInRange r p (const 0) x
apply (Instruction Toggle r) p x = applyIfInRange r p (\v -> abs (v-1)) x

applyIfInRange :: Range -> (Int, Int) -> (Int -> Int) -> Int -> Int
applyIfInRange range point fun val = if range `contains` point then fun val else val
