module Day10.Day10
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

type Point = (Int, Int)
type Grid = [Point]
data Vis = Vis { source   :: Point 
               , distance :: Double }
               deriving Show

precision = 4 :: Int -- dp
eps = 1.0 / i2d (10 ^ precision) :: Double


part1 :: String -> String
part1 = show . snd . bestVisibility . parseInput

part2 :: String -> String
part2 x = show $ nearestAngularObservers (parseInput x) (0,3) 


bestVisibility :: Grid -> (Point, Int)
bestVisibility grid = ((0,0),0)

dist :: Point -> Point -> Double
dist from to = sqrt $ sum $ map (**2) ab
  where
    ab = map (\f -> fromIntegral (f to - f from)) [fst,snd]

gridVisibility :: Grid -> Map Point Vis
gridVisibility grid = Map.empty --Map.fromList $ map (\p -> (p, nearestAngularObservers grid p)) grid

nearestAngularObservers :: Grid -> Point -> Map Double Vis
nearestAngularObservers grid tgt = resultMap 
  where
    others = filter (/=tgt) grid
    angDists = map (\x -> (angle x tgt, (vis x $ dist x tgt))) others
    resultMap = foldl (\res x -> Map.insertWith 
                  (\new cur -> if distance new < distance cur then new else cur) (fst x) (snd x) res) 
                Map.empty angDists 
      
    
vis :: Point -> Double -> Vis
vis pt dist = Vis { source=pt, distance=dist }

angle :: Point -> Point -> Double
angle from to = roundTo precision $ atan2 ((i2d $ snd to) - (i2d $ snd from)) ((i2d $ fst to) - (i2d $ fst from))

approxEq :: Double -> Double -> Bool
approxEq x0 x1 = (abs (x1 - x0)) < eps 

parseInput :: String -> Grid
parseInput x = foldl1 (++) flat
  where
    rows = zip [0..] $ lines x
    indexed = map (\(r,row) -> (r, (filter (\(_,x) -> x=='#') $ zip [0..] row))) rows
    flat = map (\(r,ast) -> map (\(c,_) -> (c,r)) ast) indexed 


-- Tests 
tests = []

