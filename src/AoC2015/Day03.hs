module AoC2015.Day03 (
  partA, partB
)
where
import Challenge
import Data.List (nub)
import Data.Char (isSpace)

{-
--- Day 3: Perfectly Spherical Houses in a Vacuum ---

Santa is delivering presents to an infinite two-dimensional grid of houses.

He begins by delivering a present to the house at his starting location, and
then an elf at the North Pole calls him via radio and tells him where to move
next. Moves are always exactly one house to the north (^), south (v), east (>),
or west (<). After each move, he delivers another present to the house at his
new location.

However, the elf back at the north pole has had a little too much eggnog, and so
his directions are a little off, and Santa ends up visiting some houses more
than once. How many houses receive at least one present?

For example:

    > delivers presents to 2 houses: one at the starting location, and one to
    > the east. ^>v< delivers presents to 4 houses in a square, including twice
    > to the house at his starting/ending location. ^v^v^v^v^v delivers a bunch
    > of presents to some very lucky children at only 2 houses.
-}
partA = Challenge uniqueVisitedHouses

-- Position on a 2D grid, does not determine where origin is: (X, Y)
newtype GridPosition = GridPos (Int, Int) deriving(Eq)

-- Moves on a 2D grid: (N, E)
newtype GridMove = GridMov (Int, Int)

squareOrigin :: GridPosition
squareOrigin = GridPos (0,0)

asciiSquareMove :: Char -> GridMove
asciiSquareMove x = case x of
  'v' -> GridMov (0, -1)
  '^' -> GridMov (0, 1)
  '>' -> GridMov (1, 0)
  '<' -> GridMov (-1, 0)
  _ -> GridMov (0, 0)

moveSquare :: GridPosition -> GridMove -> GridPosition
moveSquare (GridPos (ox, oy)) (GridMov (x,y))  = GridPos (ox+x, oy+y)

mirrorMove :: GridMove -> GridMove
mirrorMove (GridMov (x,y)) = GridMov (-x, -y)

mirrorPos :: GridPosition -> GridPosition
mirrorPos (GridPos (x,y)) = GridPos (-x, -y)

-- apply the move but inverting North and East
moveSquareSpecular :: GridPosition -> GridMove -> GridPosition
moveSquareSpecular p m = moveSquare p (mirrorMove m)

traceSquarePath :: [GridMove] -> [GridPosition]
traceSquarePath moves = scanl moveSquare squareOrigin moves

parseDay03 :: String -> [GridMove]
parseDay03 = map asciiSquareMove

uniqueVisitedHouses :: String -> Integer
uniqueVisitedHouses = toInteger.length.nub.traceSquarePath.parseDay03

{- --- Part Two ---

The next year, to speed up the process, Santa creates a robot version of
himself, Robo-Santa, to deliver presents with him.

Santa and Robo-Santa start at the same location (delivering two presents to the
same starting house), then take turns moving based on instructions from the elf,
who is eggnoggedly reading from the same script as the previous year.

This year, how many houses receive at least one present?

For example:

    ^v delivers presents to 3 houses, because Santa goes north, and then
    Robo-Santa goes south. ^>v< now delivers presents to 3 houses, and Santa and
    Robo-Santa end up back where they started. ^v^v^v^v^v now delivers presents
    to 11 houses, with Santa going one direction and Robo-Santa going the other.
-}
partB = Challenge uniqueVisitedHouses'

movesCouples :: [GridMove] -> [(GridMove,GridMove)]
movesCouples (a:b:xs) = (a, b) : movesCouples xs
movesCouples [] = []
movesCouples _ = error "Odd number of moves"

uniqueVisitedHouses' input = toInteger.length.nub $ allVisited
  where
    allVisited = visited santa ++ visited robot
    visited xs = traceSquarePath xs
    couples = movesCouples.parseDay03.strip $ input
    santa = map fst couples
    robot = map snd couples
