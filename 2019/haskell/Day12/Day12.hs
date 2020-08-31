module Day12.Day12
( part1
, part2
, tests
)
where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Common.Util

type Point = (Int, Int, Int)

part1 :: String -> String
part1 x = show 12

part2 :: String -> String
part2 x = show 32 


asList :: Point -> [Int]
asList (x,y,z) = [x,y,z]

asPoint :: [Int] -> Point
asPoint [x,y,z,_] = (x,y,z)

parseInput :: String -> [Point]
parseInput x = map (...) rows
  where
    rows = lines x

-- Tests 
tests = [ test1 ] 


test1 _ = assertEqual 12 12

