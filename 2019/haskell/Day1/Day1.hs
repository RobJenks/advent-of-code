module Day1.Day1
( part1
, part2
, tests
)
where

import Data.List
import Control.Exception

part1 :: String -> String
part1 x = show $ applyToModules fuelReq x

part2 :: String -> String
part2 x = show $ applyToModules reqAccountingForFuel x 


applyToModules :: (Int -> Int) -> String -> Int
applyToModules f x = sum $ (map f (map read (lines x)))

fuelReq :: Int -> Int
fuelReq x = (x `div` 3) - 2

reqAccountingForFuel :: Int -> Int
reqAccountingForFuel m
  | m <= 0    = 0
  | otherwise = fuel + reqAccountingForFuel fuel
    where fuel = max 0 (fuelReq m)



-- Tests
tests = [test1, test2, test3]

test1 _ = fullReqTest 14 2
test2 _ = fullReqTest 1969 966
test3 _ = fullReqTest 100756 50346


fullReqTest x exp = assertEqual (reqAccountingForFuel x) exp

assertEqual x exp = if (x == exp) then () else error ("Error: " ++ show x ++ " != " ++ show exp)
