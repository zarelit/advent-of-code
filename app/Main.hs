module Main where

import NotQuiteLisp (santaFloor)
import Puzzle (runPuzzle)

main :: IO ()
main = do
    putStrLn "Advent of Code testbed"
    runPuzzle santaFloor "test"
