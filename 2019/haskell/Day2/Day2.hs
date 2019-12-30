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


-- Tests included in Common.Cpu
tests = []

