-- module AoC2015.Day06 (
--   partA, partB
-- )
module AoC2015.Day06-- (partA, partB)
where
import Challenge

import Data.Matrix (Matrix, matrix, elementwiseUnsafe, fromList)
import Control.DeepSeq(NFData, rnf, force)
import Data.Foldable (foldl')

import AoC2015.Day06.Types
import AoC2015.Day06.Parse(parse)

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

gridWidth, gridHeight :: Int
gridWidth = 1000
gridHeight = 1000

constGrid :: a -> Matrix a
constGrid = fromList gridWidth gridHeight . replicate (gridWidth*gridHeight)

initialGrid :: Matrix Bool
initialGrid = constGrid False

instance Monoid Op where
  mempty = Nop

instance Semigroup Op where
  _ <> On = On
  _ <> Off = Off
  x <> Nop = x
  Off <> Toggle = On
  On <> Toggle = Off
  Toggle <> Toggle = Nop
  Nop <> Toggle = Toggle

instance NFData Op where
  rnf _ = ()

apply :: Op -> Bool -> Bool
apply On = const True
apply Off = const False
apply Toggle = not
apply Nop = id

-- Does the Range of the instruction apply to this matrix coordinate?
(<?) :: Range -> (Int, Int) -> Bool
(<?) (Range start end) (x, y) = let
  (Point x1 y1) = start
  (Point x2 y2) = end
  (mx, my) = (x-1, y-1) -- translate from matrix coords (they start from 1!)
  in and [
    mx >= min x1 x2, mx <= max x1 x2,
    my >= min y1 y2, my <= max y1 y2
  ]

stepGenerator :: Instruction -> Matrix Op
stepGenerator (Instruction op r) = let
  g coord = if r <? coord then op else Nop
  in matrix gridWidth gridHeight g

allStepsMatrix :: [Instruction] -> Matrix Op
allStepsMatrix = foldl' concatOp emptyOpGrid . map stepGenerator

concatOp :: Matrix Op -> Matrix Op -> Matrix Op
concatOp = force (elementwiseUnsafe (\a b -> force (a<>b)))

emptyOpGrid :: Matrix Op
emptyOpGrid = constGrid Nop

finalGrid :: Matrix Bool -> Matrix Op -> Matrix Bool
finalGrid z op = elementwiseUnsafe apply op z

countOn :: Matrix Bool -> Integer
countOn = toInteger.foldr (\x -> if x then (+1) else id) 0

partA = Challenge $ countOn.finalGrid initialGrid.allStepsMatrix.parse

partB = undefined
