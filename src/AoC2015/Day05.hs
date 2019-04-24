module AoC2015.Day05 (
  partA, partB
)
where
import Challenge
import Data.List (group, tails)
import Text.Regex.PCRE ((=~))

{-
--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or
nice.

A nice string is one with all of the following properties:

    It contains at least three vowels (aeiou only), like aei, xazegov, or
    aeiouaeiouaeiou. It contains at least one letter that appears twice in a
    row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd). It does not
    contain the strings ab, cd, pq, or xy, even if they are part of one of the
    other requirements.

For example:

    ugknbfddgicrmopn is nice because it has at least three vowels
    (u...i...o...), a double letter (...dd...), and none of the disallowed
    substrings. aaa is nice because it has at least three vowels and a double
    letter, even though the letters used by different rules overlap.
    jchzalrnumimnmhp is naughty because it has no double letter.
    haegwjzuvuyypxyu is naughty because it contains the string xy.
    dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?

-}
partA = Challenge (countA.parse)

vowels :: String -> String
vowels = filter (`elem` "aeiou")

-- rule: word with at least three vowels
threeVowels :: String -> Bool
threeVowels xs = (length.vowels) xs >= 3

-- rule: word with at least one letter that appears two times in a row
doubleLetter :: String -> Bool
doubleLetter = any (\x -> length x >= 2).group

-- rule: does not contain ab, cd, pq, or xy anywhere in the word
notBlacklisted :: String -> Bool
notBlacklisted x = not (x =~ "ab|cd|pq|xy")

-- apply all the predicates in AND
matchAll :: [a -> Bool] -> a -> Bool
matchAll preds x = all ($ x) preds

predicatePartA :: String -> Bool
predicatePartA = matchAll [threeVowels, doubleLetter, notBlacklisted]

countMatching :: (a -> Bool) -> [a] -> Integer
countMatching p = toInteger.length.filter p

parse :: String -> [String]
parse = lines

countA :: [String] -> Integer
countA = countMatching predicatePartA

{-
--- Part Two ---

Realizing the error of his ways, Santa has switched to a better model of
determining whether a string is naughty or nice. None of the old rules apply, as
they are all clearly ridiculous.

Now, a nice string is one with all of the following properties:

    It contains a pair of any two letters that appears at least twice in the
    string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like
    aaa (aa, but it overlaps). It contains at least one letter which repeats
    with exactly one letter between them, like xyx, abcdefeghi (efe), or even
    aaa.

For example:

    qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a
    letter that repeats with exactly one letter between them (zxz). xxyxx is
    nice because it has a pair that appears twice and a letter that repeats with
    one between, even though the letters used by each rule overlap.
    uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a
    single letter between them. ieodomkazucvgmuy is naughty because it has a
    repeating letter with one between (odo), but no pair that appears twice.

How many strings are nice under these new rules?
 -}
partB = Challenge (countB.parse)

repeatingPair :: String -> Bool
repeatingPair x = x =~ "(..).*\\1"

repeatingLetterWithSeparator :: String -> Bool
repeatingLetterWithSeparator x = x =~ "(.).\\1"

predicatePartB :: String -> Bool
predicatePartB = matchAll [repeatingPair, repeatingLetterWithSeparator]

countB :: [String] -> Integer
countB = countMatching predicatePartB
