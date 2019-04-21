module AoC2015(
  challenges
) where

import Challenge
import qualified AoC2015.Day01
import qualified AoC2015.Day02
import qualified AoC2015.Day03
import qualified AoC2015.Day04

year = 2015

challenges :: [(Issue, Challenge)]
challenges = [
  ((year, 1, "A"), AoC2015.Day01.partA),
  ((year, 1, "B"), AoC2015.Day01.partB),
  ((year, 2, "A"), AoC2015.Day02.partA),
  ((year, 2, "B"), AoC2015.Day02.partB),
  ((year, 3, "A"), AoC2015.Day03.partA),
  ((year, 3, "B"), AoC2015.Day03.partB),
  ((year, 4, "A"), AoC2015.Day04.partA),
  ((year, 4, "B"), AoC2015.Day04.partB)
  ]
