module Day9.Day9
( part1
, part2
, tests
)
where

import Data.List
import Data.Char
import Common.Util

part1 :: String -> String
part1 x = "A"

part2 :: String -> String
part2 x = "B"


parseInput :: String -> [Int]
parseInput = map digitToInt . filter (/= '\n')  


-- Tests 
tests = []

