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
zipPadded :: a -> b -> [a] -> [b] -> [(a,b)]
zipPadded a b (x:xs) (y:ys) = (x,y) : zipPadded a b xs ys
zipPadded a _ [] ys = zip (repeat a) ys
zipPadded _ b xs [] = zip xs (repeat b)

