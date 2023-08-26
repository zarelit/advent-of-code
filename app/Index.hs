module Index where

import Data.Map (Map, fromList, keys)
import DoesntHeHaveInternElvesForThis qualified
import IWasToldThereWouldBeNoMath qualified
import NotQuiteLisp qualified
import PerfectlySphericalHousesInAVacuum qualified
import ProbablyAFireHazard qualified
import TheIdealStockingStuffer qualified

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
        , ("2015-12-04 a", ("2015-12-04", TheIdealStockingStuffer.partA))
        , ("2015-12-04 b", ("2015-12-04", TheIdealStockingStuffer.partB))
        , ("2015-12-05 a", ("2015-12-05", DoesntHeHaveInternElvesForThis.partA))
        , ("2015-12-05 b", ("2015-12-05", DoesntHeHaveInternElvesForThis.partB))
        , ("2015-12-06 a", ("2015-12-06", ProbablyAFireHazard.partA))
        , ("2015-12-06 b", ("2015-12-06", ProbablyAFireHazard.partB))
        ]
