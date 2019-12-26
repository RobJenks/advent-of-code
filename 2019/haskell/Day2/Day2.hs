module Day2.Day2
( part1
, part2
, tests
)
where

import Data.Foldable
import Data.List
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Control.Exception


type Tape = Seq Int

part1 :: String -> String
part1 x = show $ head $ toList $ execute $ primeTape $ parseInput x

part2 :: String -> IO String
part2 x = do
 return "Not complete"


execute :: Tape -> Tape
execute tape = step tape 0

step :: Tape -> Int -> Tape
step tape ip = result
  where 
    op = Seq.index tape ip
    result = case op of
      1 -> step (opAdd tape (get 3 tape (ip+1))) (ip+4)
      2 -> step (opMult tape (get 3 tape (ip+1))) (ip+4)
           
      99 -> tape
      _ -> error ("Unknown opcode " ++ show op)


get :: Int -> Tape -> Int -> [Int]
get n tape ip = map (Seq.index tape) (map (+ ip) [0..(n-1)])

set :: Tape -> Int -> Int -> Tape
set tape ix val = Seq.update ix val tape

opAdd :: Tape -> [Int] -> Tape
opAdd tape arg = binaryIndexedOp tape arg sum

opMult :: Tape -> [Int] -> Tape
opMult tape arg = binaryIndexedOp tape arg product
                   
binaryIndexedOp :: Tape -> [Int] -> ([Int] -> Int) -> Tape
binaryIndexedOp tape arg f = set 
                  tape
                  (arg !! 2)
                  (f (map (Seq.index tape) (take 2 arg)))
                  
primeTape :: Tape -> Tape
primeTape tape = set (set tape 1 12) 2 2
  
parseInput :: String -> Seq Int
parseInput input = Seq.fromList $ map read (wordsWhen (== ',') input)

 
  
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'



-- Tests
tests = [test1, test2, test3, test4]

test1 _ = runTest [1,0,0,0,99] [2,0,0,0,99]
test2 _ = runTest [2,3,0,3,99] [2,3,0,6,99]
test3 _ = runTest [2,4,4,5,99,0] [2,4,4,5,99,9801]
test4 _ = runTest [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99]


runTest :: [Int] -> [Int] -> ()
runTest input exp = assertEqual (execute $ Seq.fromList input) (Seq.fromList exp)


assertEqual x exp = if (x == exp) then () else error ("Error: " ++ show x ++ " != " ++ show exp)
