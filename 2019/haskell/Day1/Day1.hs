module Day1.Day1
( part1
, part2
)
where

import Data.List

part1 :: String -> String
part1 x = show $ sum $ (map calcFuel (map read (lines x)))

part2 :: String -> String
part2 x = head $ lines $ x


calcFuel :: Int -> Int
calcFuel x = (x `div` 3) - 2




