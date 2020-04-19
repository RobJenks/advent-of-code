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
               deriving (Show, Ord, Eq)

precision = 6 :: Int -- dp
eps = 1.0 / i2d (10 ^ precision) :: Double
ppi = roundTo precision pi

part1 :: String -> String
part1 x = (show pt) ++ ", " ++ (show count) ++ " visible"
  where
    (pt, count) = bestVisibility $ parseInput x

part2 :: String -> String
part2 x = show $ ((fst target * 100) + snd target) 
  where
    station = (20,18)
    target = head $ drop 199 $ orderedElimination station $ parseInput x


bestVisibility :: Grid -> (Point, Int)
bestVisibility = highestVisibilityCount . visibleCount . reverseVisibility . gridVisibility

highestVisibilityCount :: [(Point, Int)] -> (Point, Int)
highestVisibilityCount vc = foldl1 (\(rp,rc) (p,c) -> if c > rc then (p,c) else (rp,rc)) vc

visibleCount :: [(Point, [Point])] -> [(Point, Int)]
visibleCount visibility = map (\(x,vs) -> (x, length vs)) visibility

orderedElimination :: Point -> Grid -> [Point]
orderedElimination _ [] = []
orderedElimination stn grid = targets ++ (orderedElimination stn (filter (`notElem` targets) asts))
  where
    asts = delete stn grid
    directlyVisible = Map.toList $ nearestAngularObservers asts stn 
    sortReverseAngle = sort $ map (\(ang,vs) -> (normAngle ang, vs)) directlyVisible
    targets = map (source . snd) sortReverseAngle


dist :: Point -> Point -> Double
dist from to = sqrt $ sum $ map (**2) ab
  where
    ab = map (\f -> fromIntegral (f to - f from)) [fst,snd]

reverseVisibility :: [(Point, [Point])] -> [(Point, [Point])]
reverseVisibility v = Map.toList reverseKeyed
  where
    pairs = foldl1 (++)
              (map (\(subject,visible) -> 
                (map (\pt -> (pt, subject)) visible)) v) 
    reverseKeyed = foldl (\m (x0,x1) -> Map.insertWith (++) x0 [x1] m) Map.empty pairs 

getViewers :: (Point, [(Double, Vis)]) -> (Point, [Point])
getViewers (tgt, visibility) = (tgt, extractViewers visibility)
  where
    extractViewers = map (source . snd)

gridVisibility :: Grid -> [(Point, [Point])]
gridVisibility grid = map getViewers byTarget
  where
    byTarget = map (\x -> (x, Map.toList $ nearestAngularObservers grid x)) grid    

nearestAngularObservers :: Grid -> Point -> Map Double Vis
nearestAngularObservers grid tgt = resultMap 
  where
    others = filter (/=tgt) grid
    angDists = map (\x -> (normAngle $ angle x tgt, (vis x $ dist x tgt))) others
    resultMap = foldl (\res x -> Map.insertWith 
                  (\new cur -> if distance new < distance cur then new else cur) (fst x) (snd x) res) 
                Map.empty angDists 
      
    
vis :: Point -> Double -> Vis
vis pt dist = Vis { source=pt, distance=dist }

angle :: Point -> Point -> Double
angle from to = roundTo precision (((-1.0)*pi/2.0) + (atan2 ((i2d $ snd to) - (i2d $ snd from)) ((i2d $ fst to) - (i2d $ fst from))))

complAngle :: Double -> Double
complAngle = (+) pi 

approxEq :: Double -> Double -> Bool
approxEq x0 x1 = (abs (x1 - x0)) < eps 

normAngle :: Double -> Double
normAngle x
  | x < 0.0      = normAngle (x + 2*pi)
  | x >= (2*ppi) = normAngle (x - 2*pi)
  | otherwise    = roundTo precision x

parseInput :: String -> Grid
parseInput x = foldl1 (++) flat
  where
    rows = zip [0..] $ lines x
    indexed = map (\(r,row) -> (r, (filter (\(_,x) -> x=='#') $ zip [0..] row))) rows 
    flat = map (\(r,ast) -> map (\(c,_) -> (c,r)) ast) indexed 


-- Tests 
tests = [ testMaximalVisibility, testAngleCalc1, testAngleCalc2, testAngleCalc3, testAngleCalc4, 
          testEliminationOrder ]


testMaximalVisibility _ = assertEqual ((11,13), 210) (bestVisibility $ parseInput testGrid)

testAngleCalc1 _ = assertEqual (dpLow$ 0.0)          (dpLow$ normAngle $ angle (11,13) (11,14))
testAngleCalc2 _ = assertEqual (dpLow$ pi/2.0)       (dpLow$ normAngle $ angle (11,13) (10,13))
testAngleCalc3 _ = assertEqual (dpLow$ pi)           (dpLow$ normAngle $ angle (11,13) (11,12))
testAngleCalc4 _ = assertEqual (dpLow$ (3.0*pi/2.0)) (dpLow$ normAngle $ angle (11,13) (12,13))

dpLow :: Double -> Double
dpLow = roundTo 4

testEliminationOrder _ = assertEqual expected actual 
  where
    elim = orderedElimination (11,13) $ parseInput testGrid
    expected = [(1,(11,12)), (2,(12,1)), (3,(12,2)), (10,(12,8)), (20,(16,0)), (50,(16,9)),
                (100,(10,16)), (199,(9,6)), (200,(8,2)), (201,(10,9)), (299,(11,1))]
    actual = map (\(ix,_) -> (ix, elim !! (ix-1))) expected


testGrid = unlines [
  ".#..##.###...#######",
  "##.############..##.",
  ".#.######.########.#",
  ".###.#######.####.#.",
  "#####.##.#.##.###.##",
  "..#####..#.#########",
  "####################",
  "#.####....###.#.#.##",
  "##.#################",
  "#####.##.###..####..",
  "..######..##.#######",
  "####.##.####...##..#",
  ".#####..#.######.###",
  "##...#.##########...",
  "#.##########.#######",
  ".####.#.###.###.#.##",
  "....##.##.###..#####",
  ".#.#.###########.###",
  "#.#.#.#####.####.###",
  "###.##.####.##.#..##"
 ]
