module TheIdealStockingStuffer where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString, isPrefixOf, pack, singleton)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup (stimes))
import Data.String.Utils (strip)

-- --- Day 4: The Ideal Stocking Stuffer ---

-- Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically forward-thinking little girls and boys.

-- To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.

-- For example:

--     If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
--     If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....

-- mineAdventCoin :: String -> Int64 -> Integer
-- mineAdventCoin prefix threshold = let
--   -- the initial part of the hash that must match
--   discover = ByteString.toStrict $ ByteString.take threshold $ Char8.repeat '0'
--   counter = [1..]
--   guesses = map (\x -> strip prefix ++ show x) counter
--   packedGuesses = map Char8.pack guesses
--   strictHash = MD5.hash . ByteString.toStrict
--   outcomes = map (B16.encode . strictHash) packedGuesses
--   hasNotPrefix = not . ByteStringStrict.isPrefixOf discover
--   in (toInteger.length $ takeWhile hasNotPrefix outcomes) + 1

coin :: Int -> ByteString -> Int
coin difficulty prefix =
    let
        -- bytestring with enough zeroes
        challenge = stimes difficulty (singleton '0')
        -- Takes an integer and forges the Hash in base16
        forge = encode . hash . (prefix <>) . pack . show
        -- Hash all the numbers from 1 to infinity
        matches = map forge ([1 ..] :: [Int])
        foundIndex = fromMaybe 0 $ findIndex (isPrefixOf challenge) matches
     in
        foundIndex + 1 -- Numbers start from one, indexes from 0

coinFive :: ByteString -> Int
coinFive = coin 5

partA :: String -> String
partA = show . coinFive . pack . strip

-- --- Part Two ---

-- Now find one that starts with six zeroes.

coinSix :: ByteString -> Int
coinSix = coin 6

partB :: String -> String
partB = show . coinSix . pack . strip
