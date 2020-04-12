module Day8.Day8
( part1
, part2
, tests
)
where

import Data.List
import Data.Char
import Common.Util

part1 :: String -> String
part1 x = show $ ((countEl 1 best) * (countEl 2 best))
  where
    layers = layerData (parseInput x) 25 6
    zeros  = map (\x -> (countEl 0 x, x)) layers
    best   = snd $ foldl (\acc x -> if (fst x) < (fst acc) then x else acc) (999, []) zeros

part2 :: String -> String
part2 x = "B" 


layerData :: [Int] -> Int -> Int -> [[Int]]
layerData = buildLinearLayers []

buildLinearLayers :: [[Int]] -> [Int] -> Int -> Int -> [[Int]]
buildLinearLayers acc [] _ _ = acc
buildLinearLayers acc xs w h = buildLinearLayers (acc ++ [(take n xs)]) (drop n xs) w h
  where n = w * h

countFn :: Eq a => (a -> Bool) -> [a] -> Int
countFn f list = length $ filter f list

countEl :: Eq a => a -> [a] -> Int
countEl el list = countFn (== el) list

parseInput :: String -> [Int]
parseInput = map digitToInt . filter (/= '\n')  


-- Tests 
tests = [ test1 ]

test1 _ = assertEqual 1 1

