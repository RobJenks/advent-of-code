module Common.Util 
( assertEqual
, wordsWhen
, zipWithPadded
, zipPadded
, ioTrace
, ioTraceDirect

, utilTests
)
where

import Data.List
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)


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




-- Tests
utilTests = [testWordsWhen, testZipPadded]

testWordsWhen :: String -> ()
testWordsWhen _ = assertEqual (wordsWhen (=='.') "ab.c.de..f.g.") ["ab","c","de","f","g"]

testZipPadded :: String -> ()
testZipPadded _ = assertEqual (zipPadded 1 2 [3,3,3] [4,4]) [(3,4),(3,4),(3,2)]

