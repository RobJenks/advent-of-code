module Day6.Day6
( part1
, part2
, tests
)
where

import Data.Maybe
import Data.Map.Strict (Map(..), (!))
import qualified Data.Map.Strict as Map

import Common.Util


data Obj = Obj { objName     :: String
               , objChildren :: [Obj]  } deriving Show


part1 :: String -> String
part1 x = show $ orbitCount $ buildTree (parseInput x) "COM"

part2 :: String -> String
part2 x = show $ transferDistance (buildTree (parseInput x) "COM") "YOU" "SAN"


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

pathToNode :: Obj -> String -> Maybe [String]
pathToNode obj node
  | ((objName obj) == node) = Just [node]
  | (null validPaths) = Nothing
  | ((length validPaths) > 1) = error "Multiple intersecting paths"
  | otherwise = Just ([objName obj] ++ (fromJust $ head validPaths))
  where
    validPaths = ((filter isJust) . (map (\c -> pathToNode c node))) $ objChildren obj

transferDistance :: Obj -> String -> String -> Int
transferDistance sys src dst = ((length srcPath) - 1) + ((length dstPath) - 1) - (2 * common)
  where
    srcPath = fromJust $ pathToNode sys "YOU"
    dstPath = fromJust $ pathToNode sys "SAN"
    common = length $ takeWhile (\(x,y) -> x==y) $ zip srcPath dstPath



-- Tests 
tests = [ testOrbitCount, testTransferDistance ]

sampleMap1 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L" :: String
sampleMap2 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN" :: String

testOrbitCount _ = assertEqual (orbitCount $ buildTree (parseInput sampleMap1) "COM") 42

testTransferDistance _ = assertEqual (transferDistance (buildTree (parseInput sampleMap2) "COM") "YOU" "SAN") 4

