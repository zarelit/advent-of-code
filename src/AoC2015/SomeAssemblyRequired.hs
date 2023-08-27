{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module SomeAssemblyRequired where

import Common (identifier, parseToEof)
import Data.Word (Word16)
import Text.ParserCombinators.ReadP (ReadP, choice, many1, skipSpaces, string)
import qualified GHC.TypeLits as point

-- --- Day 7: Some Assembly Required ---

-- This year, Santa brought little Bobby Tables a set of wires and bitwise logic gates! Unfortunately, little Bobby is a little under the recommended age range, and he needs help assembling the circuit.

-- Each wire has an identifier (some lowercase letters) and can carry a 16-bit signal (a number from 0 to 65535). A signal is provided to each wire by a gate, another wire, or some specific value. Each wire can only get a signal from one source, but can provide its signal to multiple destinations. A gate provides no signal until all of its inputs have a signal.

-- The included instructions booklet describes how to connect the parts together: x AND y -> z means to connect wires x and y to an AND gate, and then connect its output to wire z.

-- For example:

--     123 -> x means that the signal 123 is provided to wire x.
--     x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
--     p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
--     NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.

-- Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for some reason, you'd like to emulate the circuit instead, almost all programming languages (for example, C, JavaScript, or Python) provide operators for these gates.

-- For example, here is a simple circuit:

-- 123 -> x
-- 456 -> y
-- x AND y -> d
-- x OR y -> e
-- x LSHIFT 2 -> f
-- y RSHIFT 2 -> g
-- NOT x -> h
-- NOT y -> i

-- After it is run, these are the signals on the wires:

-- d: 72
-- e: 507
-- f: 492
-- g: 114
-- h: 65412
-- i: 65079
-- x: 123
-- y: 456

-- In little Bobby's kit's instructions booklet (provided as your puzzle input), what signal is ultimately provided to wire a?

{- Data types -}
newtype Value = Const Word16 deriving (Show)
newtype Signal = Signal String deriving (Show)

data Expression = Value | And Signal Signal | Or Signal Signal | Not Signal | Left Signal Value | Right Signal Value deriving (Show)
data Assignment = Assignment Expression Signal deriving (Show)

expression :: ReadP Expression
expression =
    choice
        [
            
        ]

signal :: ReadP Signal
signal = Signal <$> identifier

assignment :: ReadP Assignment
assignment = do
    e <- expression
    _ <- string " -> "
    t <- signal
    _ <- skipSpaces
    return $ Assignment e t

booklet :: ReadP [Assignment]
booklet = many1 assignment

partA :: String -> String
partA = show . parseToEof booklet

{-
This is a problem that can be solved by using a fixed point of a function.
The function takes the list of assignments and a set of signals with known values.
An assignment that can be satisfied is taken and it is resolved, returning a function
that does the same with one less assignment and one more known signal.
The fixed-point combinator will return once it's not possible to solve any more assignments.
-}
