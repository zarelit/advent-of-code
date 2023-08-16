module Common where

import Data.Char (isDigit, isSpace)
import Text.ParserCombinators.ReadP (ReadP, many, many1, readP_to_S, satisfy)

-- Consider AoC input well-formed, take first
parse :: ReadP a -> String -> a
parse parser = fst . head . readP_to_S parser

-- parse an unsigned number
unsignedNumber :: Read a => ReadP a
unsignedNumber = read <$> many1 (satisfy isDigit)

-- parse some whitespace
optionalWhitespace :: ReadP String
optionalWhitespace = many (satisfy isSpace)
