module Day8.Day8
( part1
, part2
, tests
)
where

import Data.List
import Data.Char
import Common.Util

width       = 25 :: Int
height      =  6 :: Int
black       =  0 :: Int
white       =  1 :: Int
transparent =  2 :: Int

part1 :: String -> String
part1 x = show $ ((countEl 1 best) * (countEl 2 best))
  where
    layers = layerData (parseInput x) (width * height)
    zeros  = map (\x -> (countEl 0 x, x)) layers
    best   = snd $ foldl (\acc x -> if (fst x) < (fst acc) then x else acc) (999, []) zeros

part2 :: String -> String
part2 x = "\n" ++ (unlines $ rendered)
  where
    decoded = decode $ layerData (parseInput x) (width * height)
    lines = layerData decoded width
    rendered = map (\line -> (map (\pixel -> if pixel == white then '#' else ' ') line)) lines


layerData :: [Int] -> Int -> [[Int]]
layerData [] _ = [] 
layerData xs n = [take n xs] ++ layerData (drop n xs) n

decode :: [[Int]] -> [Int]
decode layers
  | null $ filter (not . null) layers = []
  | otherwise = [pixel] ++ (decode remainder)
      where
        col = map head layers
        remainder = map tail layers
        pixel = maybe transparent id (find (/=transparent) col)


countFn :: Eq a => (a -> Bool) -> [a] -> Int
countFn f list = length $ filter f list

countEl :: Eq a => a -> [a] -> Int
countEl el list = countFn (== el) list

parseInput :: String -> [Int]
parseInput = map digitToInt . filter (/= '\n')  


-- Tests 
tests = []

