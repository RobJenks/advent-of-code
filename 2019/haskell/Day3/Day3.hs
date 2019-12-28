module Day3.Day3
( part1
, part2
, tests
)
where

import Data.List
import Data.Ord
import Data.Set (Set(..))
import qualified Data.Set as Set

type Coord = (Int, Int)
origin = (0,0) :: Coord 

part1 :: String -> String
part1 x = show best ++ " -> " ++ (show $ mdist best)
  where 
    cv = map coverage (parseInput x)
    best = closest $ intersections (cv !! 0) (cv !! 1)


part2 :: String -> String
part2 x = "Not completed"


closest :: [(Int, Int)] -> (Int, Int)
closest ps = head $ sortBy (comparing mdist) ps


intersections :: Set Coord -> Set Coord -> [(Int, Int)]
intersections x0 x1 = filter (\x -> x /= (0,0)) 
                      (Set.toList $ Set.intersection x0 x1)

coverage :: [Coord] -> Set Coord
coverage ps = Set.fromList (scanl move origin ps)
                  
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

