module Main where

import qualified ChallengeLauncher as CL

main :: IO ()
main = do
  putStrLn "Hello, Advent Of Code!"
  mapM_ CL.print CL.issues
