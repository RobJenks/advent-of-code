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
part1 x = show $ head $ reverse $ Cpu.outputState $ executeDiagnostics x


part2 :: String -> String
part2 _ = "Not completed" 


executeDiagnostics :: String -> State
executeDiagnostics x = case result of 
  Left e -> error ("Execution failed: " ++ e)
  Right state -> state
  where 
    result = Cpu.execute (Cpu.parseInput x) 1




-- Tests
tests = [performDiagnosticTests]

performDiagnosticTests x = head [
  assertEqual (head reversedOutput) 16225258,
  assertEqual (filter (/=0) $ drop 1 reversedOutput) [] ]
  where
    reversedOutput = reverse $ Cpu.outputState $ executeDiagnostics x




