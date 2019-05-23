module AoC2015(
  challenges
) where

import Challenge
import qualified AoC2015.Day01
import qualified AoC2015.Day02
import qualified AoC2015.Day03
import qualified AoC2015.Day04
import qualified AoC2015.Day05
import qualified AoC2015.Day06

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
  ((year, 4, "B"), AoC2015.Day04.partB),
  ((year, 5, "A"), AoC2015.Day05.partA),
  ((year, 5, "B"), AoC2015.Day05.partB),
  ((year, 6, "A"), AoC2015.Day06.partA),
  ((year, 6, "B"), AoC2015.Day06.partB)
  ]
