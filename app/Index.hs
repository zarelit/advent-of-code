module Index where

import Data.Map (Map, fromList, keys)
import IWasToldThereWouldBeNoMath qualified
import NotQuiteLisp qualified
import PerfectlySphericalHousesInAVacuum qualified

type Name = String
type InputName = String
type Puzzle = (String -> String)

solvedDays :: [String]
solvedDays = keys solved

solved :: Map Name (InputName, Puzzle)
solved =
    fromList
        [ ("2015-12-01 a", ("2015-12-01", NotQuiteLisp.partA))
        , ("2015-12-01 b", ("2015-12-01", NotQuiteLisp.partB))
        , ("2015-12-02 a", ("2015-12-02", IWasToldThereWouldBeNoMath.partA))
        , ("2015-12-02 b", ("2015-12-02", IWasToldThereWouldBeNoMath.partB))
        , ("2015-12-03 a", ("2015-12-03", PerfectlySphericalHousesInAVacuum.partA))
        , ("2015-12-03 b", ("2015-12-03", PerfectlySphericalHousesInAVacuum.partB))
        ]
