module Common.Util  -- export all 
where

import Data.List
import Control.Exception


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
zipWithPadded f a b (x:xs) (y:ys) = (f a b) : zipWithPadded f a b xs ys
zipWithPadded f a _ [] ys = zipWith f (repeat a) ys
zipWithPadded f _ b xs [] = zipWith f xs (repeat b)

zipPadded :: a -> b -> [a] -> [b] -> [(a,b)]
zipPadded a b xa xb = zipWithPadded (\a b -> (a,b)) a b xa xb



