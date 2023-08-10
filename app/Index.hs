module Index where

import Data.Map (Map, fromList, keys)
import NotQuiteLisp qualified

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
        ]
