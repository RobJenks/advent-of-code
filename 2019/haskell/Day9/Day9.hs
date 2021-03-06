module Day9.Day9
( part1
, part2
, tests
)
where

import Data.List
import Data.Char
import Common.Cpu (Tape, State)
import qualified Common.Cpu as Cpu
import Common.Util

part1 :: String -> String
part1 x = show $ head $ Cpu.outputState $ execute x [1]

part2 :: String -> String
part2 x = show $ head $ Cpu.outputState $ execute x [2]


execute :: String -> [Int] -> State
execute prog input = case result of 
  Left e -> error ("Execution failed: " ++ e)
  Right state -> state
  where 
    result = Cpu.execute (Cpu.parseInput prog) input


-- Tests 
tests = [ runDiagnosticTests ]

runDiagnosticTests x = assertEqual (Cpu.outputState $ execute x [1]) [2518058886]

