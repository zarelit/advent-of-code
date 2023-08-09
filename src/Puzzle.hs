module Puzzle where

type Name = String
type Input = String
data Puzzle = Puzzle Name (Input -> String)

runPuzzle :: Puzzle -> String -> IO ()
runPuzzle (Puzzle name f) input = do
    putStr $ name ++ " " ++ f input
