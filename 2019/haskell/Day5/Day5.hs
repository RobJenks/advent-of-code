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
part1 _ = "Not completed"

part2 :: String -> String
part2 _ = "Not completed" 



-- Tests
tests = [test1]

test1 :: String -> ()
test1 _ = Cpu.testProgram [3,0,4,0,99] 12 [12,0,4,0,99] [12] 


