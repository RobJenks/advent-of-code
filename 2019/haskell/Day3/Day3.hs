module Day3.Day3
( part1
, part2
, tests
)
where

import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map,(!))
import qualified Data.Map as Map

type Coord = (Int, Int)
origin = (0,0) :: Coord 


part1 :: String -> String
part1 x = show best ++ " -> " ++ (show $ mdist best)
  where 
    cv = map coverage (parseInput x)
    best = closest $ intersections (cv !! 0) (cv !! 1)


part2 :: String -> String
part2 x = show $ shortest intersect (snd $ cv !! 0) (snd $ cv !! 1)
  where
    cv = map distCoverage (parseInput x)
    intersect = intersections (fst $ cv !! 0) (fst $ cv !! 1)


closest :: [Coord] -> Coord
closest ps = head $ sortBy (comparing mdist) ps

shortest :: [Coord] -> Map Coord Int -> Map Coord Int -> Int
shortest ps d0 d1 = head $ sort $ map (\p -> (d0!p) + (d1!p)) ps

intersections :: Set Coord -> Set Coord -> [(Int, Int)]
intersections x0 x1 = filter (\x -> x /= (0,0)) 
                      (Set.toList $ Set.intersection x0 x1)

coverage :: [Coord] -> Set Coord
coverage ps = Set.fromList (scanl move origin ps)

distCoverage :: [Coord] -> (Set Coord, Map Coord Int)
distCoverage ps = (Set.fromList (map fst path), Map.fromListWith min path)
  where
    path = (scanl (\(p,i) d -> ((move p d), i+1)) (origin, 0) ps) :: [(Coord, Int)]
                  
move :: Coord -> Coord -> Coord
move (px,py) (dx,dy) = (px + dx, py + dy)
                 
mdist :: Coord -> Int
mdist (x,y) = x + y 
                  

parseInput :: String -> [[Coord]]
parseInput x = map  parseLine (lines x)

parseLine :: String -> [Coord]
parseLine x = foldl (++) [] 
                (map (\comp -> replicate (read $ tail comp) (dir $ head comp))
                components)
  where
    components = wordsWhen (== ',') x


dir :: Char -> Coord
dir c = case c of
  'U' -> (0, 1)
  'D' -> (0,-1)
  'L' -> (-1,0)
  'R' -> (1, 0)
  _   -> error "Invalid direction"


zipPadded :: a -> b -> [a] -> [b] -> [(a,b)]
zipPadded a b (x:xs) (y:ys) = (x,y) : zipPadded a b xs ys
zipPadded a _ [] ys = zip (repeat a) ys
zipPadded _ b xs [] = zip xs (repeat b)


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'


-- Tests
tests = [test1]

test1 _ = assertEqual 4 4


assertEqual x exp = if (x == exp) then () else error ("Error: " ++ show x ++ " != " ++ show exp)

