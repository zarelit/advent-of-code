module Main where

import Control.Monad (when)
import Data.List (intercalate)
import Data.Map (lookup)
import Data.Maybe (fromJust)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Index (Name, solved, solvedDays)
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

inputPath :: String -> FilePath
inputPath name = "data/" </> name <> ".txt"
