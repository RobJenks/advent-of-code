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

import Common.Cpu (Tape,State)
import qualified Common.Cpu as Cpu
import Common.Util


part1 :: String -> String
part1 x = case Cpu.executeNoInput $ Cpu.primeTape [(1,12),(2,2)] $ Cpu.parseInput x of
  Left e -> error ("Execution failed: " ++ e)
  Right state -> show $ head $ toList $ Cpu.tapeState $ state

part2 :: String -> String
part2 x = show 
           (((map (\(p,res) -> ((show p) ++ " -> " ++ show (100*(fst p) + (snd p))))) . 
             (filter (\(p,res) -> either (\_ -> False) (\t -> ((Seq.index (Cpu.tapeState t) 0) == 19690720)) res)) .
             (map (\p -> (p, Cpu.executeNoInput $ Cpu.primeTape [(1,fst p),(2,snd p)] (Cpu.parseInput x)))))
             (nounVerbPairs 0 99))



nounVerbPairs :: Int -> Int -> [(Int, Int)]
nounVerbPairs l h = [ (x,y) | x <- [l..h], y <- [l..h] ]



-- Tests
tests = [test1, test2, test3, test4]

test1 _ = tapeOnlyTest [1,0,0,0,99] [2,0,0,0,99]
test2 _ = tapeOnlyTest [2,3,0,3,99] [2,3,0,6,99]
test3 _ = tapeOnlyTest [2,4,4,5,99,0] [2,4,4,5,99,9801]
test4 _ = tapeOnlyTest [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99]


tapeOnlyTest :: [Int] -> [Int] -> ()
tapeOnlyTest input exp = Cpu.testProgram input 0 exp []

