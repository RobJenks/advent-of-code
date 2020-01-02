module Day6.Day6
( part1
, part2
, tests
)
where

import Data.Map.Strict (Map(..), (!))
import qualified Data.Map.Strict as Map

import Common.Util


data Obj = Obj { objName     :: String
               , objChildren :: [Obj]  } deriving Show



part1 :: String -> String
part1 x = show $ orbitCount $ buildTree (parseInput x) "COM"

part2 :: String -> String
part2 x = "Not completed"


buildTree :: Map String [String] -> String -> Obj
buildTree m name = Obj { objName = name, objChildren = (map (buildTree m) children) }
  where
    children = Map.findWithDefault [] name m


parseInput :: String -> Map String [String]
parseInput x = Map.fromListWith (++) orbits
  where
    lns = map (wordsWhen (==')')) $ lines x
    orbits = map (\x -> (head x, tail x)) lns 

hasChildren :: Obj -> Bool
hasChildren obj = not $ null $ objChildren $ obj

directChildCount :: Obj -> Int
directChildCount obj = length $ objChildren $ obj

orbitCount :: Obj -> Int
orbitCount obj = aggregateDepth 0 obj 

aggregateDepth :: Int -> Obj -> Int
aggregateDepth depth obj = depth + (sum $ map (aggregateDepth (depth+1)) (objChildren obj))





-- Tests 
tests = [testSample]

sampleMap = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L" :: String

testSample x = assertEqual (orbitCount $ buildTree (parseInput sampleMap) "COM") 42


