module Day4.Day4
( part1
, part2
, tests
)
where

import Data.List
import Data.Char
import Data.Bits

range = (172930, 683082) :: (Int, Int)


part1 :: String -> String
part1 _ = show $ length $ filter id (map valid [(fst range)..(snd range)])

part2 :: String -> String
part2 _ = show $ length $ filter id (map strictValid [(fst range)..(snd range)]) 


valid :: Int -> Bool
valid n = isAscending && hasDouble
  where
    (_, isAscending, hasDouble) = foldl (\(last,ascending,double) c -> (c, 
                                                                       (ascending .&. (c >= last)), 
                                                                       (double .|. (c == last))))
                                    ((chr 0),True,False) (show n)

strictValid :: Int -> Bool
strictValid n = (isAscending s) && (hasStrictDouble s)
  where
    s = show n


hasStrictDouble :: String -> Bool
hasStrictDouble s = not $ null $ ((filter (== 2) . map length) (group s))

isAscending :: String -> Bool
isAscending (x0:x1:xs) = (x1 >= x0) && (isAscending (x1:xs))
isAscending _ = True


-- Tests
tests = []

