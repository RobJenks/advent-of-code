module Day2.Day2
( part1
, part2
, tests
)
where

import Data.Foldable
import Data.Either
import Data.List
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Control.Exception


type Tape = Seq Int
type State = Either String Tape
maxCycles = 10000 :: Int

part1 :: String -> String
part1 x = case execute $ primeTape [(1,12),(2,2)] $ parseInput x of
  Left e -> error ("Execution failed: " ++ e)
  Right tape -> show $ head $ toList tape

part2 :: String -> String
part2 x = show 
           (((map (\(p,res) -> ((show p) ++ " -> " ++ show (100*(fst p) + (snd p))))) . 
             (filter (\(p,res) -> either (\_ -> False) (\t -> ((Seq.index t 0) == 19690720)) res)) .
             (map (\p -> (p, execute $ primeTape [(1,fst p),(2,snd p)] (parseInput x)))))
             (nounVerbPairs 0 99))


execute :: Tape -> State
execute tape = executeFor tape maxCycles

executeFor :: Tape -> Int -> State
executeFor tape cpuCycles = step (ok tape) 0 cpuCycles

step :: State -> Int -> Int -> State
step state ip cpuTime = result
  where 
    result = case state of 
      Left e -> err e
      Right tape -> cycleResult
        where 
          op 
            | cpuTime > 0 = Seq.index tape ip
            | otherwise   = -1
          cycleResult =
            case op of
              1 -> step (ok $ opAdd tape (get 3 tape (ip+1))) (ip+4) (cpuTime-1)
              2 -> step (ok $ opMult tape (get 3 tape (ip+1))) (ip+4) (cpuTime-1)
           
              99 -> ok tape
              -1 -> err "Out of CPU cycles"

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
                  
ok :: Tape -> State
ok tape = Right tape

err :: String -> State
err e = Left e

primeTape :: [(Int, Int)] -> Tape -> Tape
primeTape vals tape = foldl (\t x -> (set t (fst x) (snd x))) tape vals
  
parseInput :: String -> Tape
parseInput input = Seq.fromList $ map read (wordsWhen (== ',') input)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'
                    
nounVerbPairs :: Int -> Int -> [(Int, Int)]
nounVerbPairs l h = [ (x,y) | x <- [l..h], y <- [l..h] ]



-- Tests
tests = [test1, test2, test3, test4]

test1 _ = runTest [1,0,0,0,99] [2,0,0,0,99]
test2 _ = runTest [2,3,0,3,99] [2,3,0,6,99]
test3 _ = runTest [2,4,4,5,99,0] [2,4,4,5,99,9801]
test4 _ = runTest [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99]


runTest :: [Int] -> [Int] -> ()
runTest input exp = case (execute $ Seq.fromList input) of 
  Left e -> error ("Test failed: " ++ e)
  Right tape -> assertEqual tape (Seq.fromList exp)


assertEqual x exp = if (x == exp) then () else error ("Error: " ++ show x ++ " != " ++ show exp)

