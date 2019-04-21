module AoC2015.Day04 (
  partA, partB
)
where
import Challenge
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString as ByteStringStrict
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import GHC.Int (Int64)

{-
--- Day 4: The Ideal Stocking Stuffer ---

Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
gifts for all the economically forward-thinking little girls and boys.

To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
least five zeroes. The input to the MD5 hash is some secret key (your puzzle
input, given below) followed by a number in decimal. To mine AdventCoins, you
must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
that produces such a hash.

For example:
Integer
    If your secret key is abcdef, the answer is 609043, because the MD5 hash of
    abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
    such number to do so. If your secret key is pqrstuv, the lowest number it
    combines with to make an MD5 hash starting with five zeroes is 1048970; that
    is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
-}
partA = Challenge adventCoinFive

adventCoinFive :: String -> Integer
adventCoinFive = (flip mineAdventCoin) 5

mineAdventCoin :: String -> Int64 -> Integer
mineAdventCoin prefix threshold = let
  -- the initial part of the hash that must match
  discover = ByteString.toStrict $ ByteString.take threshold $ Char8.repeat '0'
  counter = [1..]
  guesses = map (\x -> (strip prefix) ++ show x) counter
  packedGuesses = map (Char8.pack) guesses
  strictHash = (MD5.hash).(ByteString.toStrict)
  outcomes = map ((B16.encode).strictHash) packedGuesses
  hasNotPrefix = not.(ByteStringStrict.isPrefixOf discover)
  in (toInteger.length $ takeWhile hasNotPrefix outcomes) + 1

{-
--- Part Two ---

Now find one that starts with six zeroes.
-}
partB = Challenge adventCoinSix

adventCoinSix :: String -> Integer
adventCoinSix = (flip mineAdventCoin) 6
