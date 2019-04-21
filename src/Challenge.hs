module Challenge(
  Issue,
  Challenge(Challenge),
  strip
) where

-- Issue is the unique identifier for a day of the AoC
-- e.g. 2015, 22, B means Day 22, part B of the 2015 AoC
type Issue = (Int, Int, String)
newtype Challenge = Challenge (String -> Integer)

-- utility to strip newlines at the end of a line
strip :: String -> String
strip = head.lines
