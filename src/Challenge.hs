module Challenge(
  Issue,
  Challenge(Challenge),
) where

-- Issue is the unique identifier for a day of the AoC
-- e.g. 2015, 22, B means Day 22, part B of the 2015 AoC
type Issue = (Int, Int, String)
data Challenge = Challenge (String -> Integer)
