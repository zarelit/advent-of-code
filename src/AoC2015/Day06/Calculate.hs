module AoC2015.Day06.Calculate (
countOn, runInstructions
) where

import AoC2015.Day06.Types
import Data.Matrix (Matrix, matrix, elementwiseUnsafe, fromList)
import Control.DeepSeq(NFData, rnf, force)
import Data.Foldable (foldl')

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

runInstructions :: [Instruction] -> Matrix Bool
runInstructions = finalGrid initialGrid.allStepsMatrix
