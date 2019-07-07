module AoC2015.Day06.Parse (
parse
) where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha)
import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (mapMaybe)
import AoC2015.Day06.Types

---- Parsing of the instructions

instruction :: ReadP Instruction
instruction = do
  op <- operation
  char ' '
  r <- range
  return $ Instruction op r

operation :: ReadP Op
operation = let
  -- parse any string with spaces and letters and let opLookup be the source
  -- of truth for the String reprentation of Op
  opStr = many1 (satisfy isAlpha <|> satisfy (== ' '))
  in do
    op <- opStr
    maybe pfail return $ opLookup op

opLookup :: String -> Maybe Op
opLookup "turn on" = Just On
opLookup "turn off" = Just Off
opLookup "toggle" = Just Toggle
opLookup _ = Nothing

range :: ReadP Range
range = do
  a <- point
  string " through "
  b <- point
  return $ Range a b

point :: ReadP Point
point = let
  number = fmap read (many1 $ satisfy isDigit)
  in do
    a <- number
    satisfy (== ',')
    b <- number
    return $ Point a b

parseInstruction :: String -> Maybe Instruction
parseInstruction s = let
  isComplete (_, rest) = rest == ""
  in fst <$> find isComplete (readP_to_S instruction s)

parse :: String -> [Instruction]
parse = mapMaybe parseInstruction . lines
