module Main where

import Control.Monad (when)
import Data.List (intercalate)
import Data.Map (Map, fromList, keys, lookup)
import Data.Maybe (fromJust)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Prelude hiding (lookup)

main :: IO ()
main = do
    putStrLn "Advent of Code testbed"
    args <- getArgs
    exe <- getProgName
    when (null args) $ do
        putStrLn $ "Usage: " ++ exe ++ " EXERCISE"
        putStrLn $ "Solutions available: " ++ intercalate ", " solvedDays
        exitFailure
    let name = unwords args
    putStrLn $ "Chosen puzzle " ++ name
    printSolution name

printSolution :: Name -> IO ()
printSolution name = do
    let puzzle = lookup name solved
    when (null puzzle) $ do
        putStrLn "Not implemented"
        exitFailure

    let (i, f) = fromJust puzzle
    inputContent <- readFile $ inputPath i
    putStrLn $ name ++ ": " ++ f inputContent

type Name = String
type InputName = String
type Puzzle = (String -> String)

inputPath :: String -> FilePath
inputPath name = "data/" </> name <> ".txt"

solvedDays :: [String]
solvedDays = keys solved

solved :: Map Name (InputName, Puzzle)
solved =
    fromList
        [ ("2015-12-01 a", ("2015-12-01", id))
        ]
