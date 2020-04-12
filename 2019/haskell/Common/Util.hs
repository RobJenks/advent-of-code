module Common.Util 
( assertEqual
, wordsWhen
, zipWithPadded
, zipPadded
, rotate
, ioTrace
, ioTraceDirect
, stack
, io

, utilTests
)
where

import Data.List
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (traceStack)

-- Test assertions
assertEqual x exp = if (x == exp) then () else error ("Error: " ++ show x ++ " != " ++ show exp)

-- Split string on the given predicate
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'

-- Zip two lists with specified padding when unequal lengths
zipWithPadded :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithPadded f a b (x:xs) (y:ys) = (f x y) : zipWithPadded f a b xs ys
zipWithPadded f a _ [] ys = zipWith f (repeat a) ys
zipWithPadded f _ b xs [] = zipWith f xs (repeat b)

zipPadded :: a -> b -> [a] -> [b] -> [(a,b)]
zipPadded a b xa xb = zipWithPadded (\a b -> (a,b)) a b xa xb

-- Rotate a list either left or right
rotate :: Int -> [a] -> [a]
rotate n xs = take lxs . drop ((negate n) `mod` lxs) . cycle $ xs
  where
    lxs = length xs

-- Force eager io trace operations for debugging purposes
{-# NOINLINE ioTraceDirect #-}
ioTraceDirect :: Show a => a -> a
ioTraceDirect x = unsafePerformIO $ do
  print x
  pure x

-- Force eager io trace operations for debugging purposes
{-# NOINLINE ioTrace #-}
ioTrace :: Show a => a -> b -> b
ioTrace m x = unsafePerformIO $ do
  print m
  pure x

-- Debug output with a stack trace (reuires ghc -prof -prof-auto)
stack :: String -> a -> a
stack = traceStack

-- Convert pure code to an IO operation
io :: a -> IO a
io x = do
 return x



-- Tests
utilTests = [testWordsWhen, testZipPadded, testRotateNeg, testRotateZero, testRotatePos]

testWordsWhen :: String -> ()
testWordsWhen _ = assertEqual (wordsWhen (=='.') "ab.c.de..f.g.") ["ab","c","de","f","g"]

testZipPadded :: String -> ()
testZipPadded _ = assertEqual (zipPadded 1 2 [3,3,3] [4,4]) [(3,4),(3,4),(3,2)]

testRotateNeg :: String -> ()
testRotateNeg _ = assertEqual (rotate (negate 2) [1,2,3,4,5]) [3,4,5,1,2]

testRotateZero :: String -> ()
testRotateZero _ = assertEqual (rotate 0 [1,2,3,4,5]) [1,2,3,4,5]

testRotatePos :: String -> ()
testRotatePos _ = assertEqual (rotate 2 [1,2,3,4,5]) [4,5,1,2,3]

