import qualified Day1.Day1 as Day1
import qualified Day2.Day2 as Day2
import qualified Day3.Day3 as Day3
import qualified Day4.Day4 as Day4
import qualified Day5.Day5 as Day5
import qualified Day6.Day6 as Day6
import qualified Day7.Day7 as Day7
import qualified Day8.Day8 as Day8
import qualified Day9.Day9 as Day9

import Common.Util (io)
import qualified Common.Util as Util
import qualified Common.Cpu as Cpu

import Control.Exception

-- Unit test state for all modules
skipTests = False

-- Solutions
data Solution = Solution { day :: Int
                         , part1 :: (String -> IO String)
                         , part2 :: (String -> IO String)
                         , tests :: [(String -> ())]
                         }

main :: IO()
main = do
  let common = [ ("Util", Util.utilTests)
               , ("IntCode CPU", Cpu.cpuTests) ]

  let solutions = [ Solution { day=1, part1=io.Day1.part1, part2=io.Day1.part2, tests=Day1.tests }
                  , Solution { day=2, part1=io.Day2.part1, part2=io.Day2.part2, tests=Day2.tests }
                  , Solution { day=3, part1=io.Day3.part1, part2=io.Day3.part2, tests=Day3.tests } 
                  , Solution { day=4, part1=io.Day4.part1, part2=io.Day4.part2, tests=Day4.tests }
                  , Solution { day=5, part1=io.Day5.part1, part2=io.Day5.part2, tests=Day5.tests } 
                  , Solution { day=6, part1=io.Day6.part1, part2=io.Day6.part2, tests=Day6.tests } 
                  , Solution { day=7, part1=   Day7.part1, part2=   Day7.part2, tests=Day7.tests }
                  , Solution { day=8, part1=io.Day8.part1, part2=io.Day8.part2, tests=Day8.tests } 
                  , Solution { day=9, part1=io.Day9.part1, part2=io.Day9.part2, tests=Day9.tests } ]
  
  runCommonTests common []
  runSolutions solutions 


runSolutions :: [Solution] -> IO()
runSolutions [] = do return ()
runSolutions (x:xs) = do
  runSolution x
  runSolutions xs
  

runSolution :: Solution -> IO()
runSolution solution = do
  readInput <- try $ readFile ("Day" ++ (show $ day $ solution) ++ "/input") :: IO (Either SomeException String)
  let input = either (\e -> "") id readInput
  p1 <- (part1 solution $ input)
  p2 <- (part2 solution $ input)
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Tests : " ++ (runTests (tests solution) input))
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Part 1: " ++ p1)
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Part 2: " ++ p2 ++ "\n")


runCommonTests :: [(String, [(String -> ())])] -> String -> IO()
runCommonTests [] _ = do
  putStrLn []
  return ()
runCommonTests (t:ts) input = do
  putStrLn ("Common tests (" ++ (fst t) ++ "): " ++ (runTests (snd t) input))
  runCommonTests ts input

runTests :: [(String -> ())] -> String -> String
runTests ts input
  | skipTests = "Tests skipped"
  | otherwise = (show $ sum $ (map (\x -> if x == () then 1 else 0) (map ($ input) ts))) ++ " executed"

