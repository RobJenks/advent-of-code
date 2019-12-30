module Day5.Day5
( part1
, part2
, tests
)
where

import Data.List

import Common.Cpu (Tape, State)
import qualified Common.Cpu as Cpu
import Common.Util


part1 :: String -> String
part1 x = case result of 
  Left e -> error ("Execution failed: " ++ e)
  Right state -> show $ Cpu.outputState $ state
  where
    result = Cpu.execute (Cpu.parseInput x) 1

part2 :: String -> String
part2 _ = "Not completed" 



-- Tests
tests = [testInOut, testPositional]

testInOut :: String -> ()
testInOut _ = Cpu.testProgram [3,0,4,0,99] 12 [12,0,4,0,99] [12] 

testPositional :: String -> ()
testPositional _ = Cpu.testProgram [2,0,2,5,99,0] 0 [2,0,1,5,99,4] []




