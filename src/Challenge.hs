module Challenge(
  Issue,
  Challenge(Challenge),
  challenges
) where

import qualified Text.Printf as T
import qualified Data.Map as Map

-- Issue is the unique identifier for a day of the AoC
-- e.g. 2015, 22, B means Day 22, part B of the 2015 AoC
type Issue = (Int, Int, String)
data Challenge = Challenge (String -> Integer)

inputPath :: Issue -> FilePath
inputPath (year, day, _) = T.printf "data/%04d_%02d_input" year day

outputPath :: Issue -> FilePath
outputPath (year, day, part) = T.printf "data/%04d_%02d_%s_solved" year day part

-- All the implemented challenges
challenges :: Map.Map Issue Challenge
challenges = Map.fromList []
