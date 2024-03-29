module Common where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, eof, many1, readP_to_S, satisfy)

-- Consider AoC input well-formed, take first
parse :: ReadP a -> String -> a
parse parser = fst . head . readP_to_S parser

-- parse an unsigned number
unsignedNumber :: Read a => ReadP a
unsignedNumber = read <$> many1 (satisfy isDigit)

-- make parser eat all the input
toEof :: ReadP a -> ReadP a
toEof p = do
    x <- p
    eof
    return x

-- To use with parsers that completely cover the input
parseToEof :: ReadP a -> String -> a
parseToEof = parse . toEof
