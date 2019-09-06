module AoC2015.Day06.Calculate (
runChallengeA
) where

import AoC2015.Day06.Types
import Numeric.Matrix

-- as per the exercise definition
width = 1000
height = 1000

runChallengeA :: [Instruction] -> Integer
runChallengeA = runChallenge opA

-- runChallengeB :: [Instruction] -> Integer
-- runChallengeB = runChallenge lookupB

runChallenge :: OpFun -> [Instruction] -> Integer
runChallenge opFun xs = (toInteger.Numeric.Matrix.sum) (endMatrix opFun xs)

startMatrix :: Matrix Int
startMatrix = matrix (width, height) (const 0)

endMatrix :: OpFun -> [Instruction] -> Matrix Int
endMatrix opFun = foldl (step opFun) startMatrix

contains :: Range -> (Int, Int) -> Bool
contains (Range start end) p = let
  Point x1 y1 = start
  Point x2 y2 = end
  (xp, yp) = p
  (mxp, myp) = (xp-1, yp-1)
  in
    mxp <= max x1 x2 && mxp >= min x1 x2 &&
    myp <= max y1 y2 && myp >= min y1 y2

step :: OpFun -> Matrix Int -> Instruction -> Matrix Int
step opFun z i = mapWithIndex apply z
  where
    Instruction op r = i
    apply p = applyIfInRange r p (opFun op)

applyIfInRange :: Range -> (Int, Int) -> (Int -> Int) -> Int -> Int
applyIfInRange range point fun val = if range `contains` point then fun val else val

opA :: OpFun
opA On = const 1
opA Off = const 0
opA Toggle = \v -> abs (v-1) -- i.e. 0 becomes 1, 1 becomes 0
