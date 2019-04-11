module ChallengeLauncher(
  issues,
  print,
  solveAndPrint
) where

import qualified Text.Printf as T
import qualified Data.Map as Map
import Data.Map ((!))
import Prelude hiding (print)
import System.Directory (doesFileExist)
import Challenge

import qualified AoC2015

inputPath :: Issue -> FilePath
inputPath (year, day, _) = T.printf "data/%04d_%02d_input" year day

outputPath :: Issue -> FilePath
outputPath (year, day, part) = T.printf "data/%04d_%02d_%s_solved" year day part

-- All the implemented challenges
challenges :: Map.Map Issue Challenge
challenges = Map.fromList $ concat [
  AoC2015.challenges
  ]

issues :: [Issue]
issues = Map.keys challenges

run :: Issue -> String -> Integer
run x = exercise where
  Challenge exercise = challenges ! x

-- Print the solved exercise like (2015, 1, "A"): 232
print :: Issue -> IO ()
print x = do
  input <- readFile $ inputPath x
  putStrLn $ concat [show x, ": ", show (run x input)]

printCachedSolution :: Issue -> IO ()
printCachedSolution x = do
  solution <- readFile (outputPath x)
  putStr $ concat [show x, ": ", solution]

solveAndPrint :: Issue -> IO ()
solveAndPrint x = do
  let outPath = outputPath x
  outputExists <- doesFileExist outPath
  if outputExists
  then
    printCachedSolution x
  else
    print x
