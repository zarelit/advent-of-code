module AoC2015.Day06.Types where

data Instruction = Instruction Op Range deriving Show
data Op = Off | On | Toggle deriving Show
data Range = Range Start End deriving Show
type Start = Point
type End = Start
data Point = Point Int Int deriving (Eq, Show)
