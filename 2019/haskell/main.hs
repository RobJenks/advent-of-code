import qualified Day1.Day1 as Day1

-- Unit test state for all modules
skipTests = False

-- Solutions
data Solution = Solution { day :: Int
                         , part1 :: (String -> String)
                         , part2 :: (String -> String)
                         , tests :: [(String -> ())]
                         }

main :: IO()
main = do
  let solutions = [ Solution { day=1, part1=Day1.part1, part2=Day1.part2, tests=Day1.tests }
                  , Solution { day=1, part1=Day1.part1, part2=Day1.part2, tests=Day1.tests } ]
  -- putStrLn $ show $ (map id (Day1.tests False))  
  runSolutions solutions 

  

runSolutions :: [Solution] -> IO()
runSolutions [] = do return ()
runSolutions (x:xs) = do
  runSolution x
  runSolutions xs
  

runSolution :: Solution -> IO()
runSolution solution = do
  input <- readFile ("Day" ++ (show $ day $ solution) ++ "/input")
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Tests : " ++ (runTests solution input))
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Part 1: " ++ (part1 solution $ input))
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Part 2: " ++ (part2 solution $ input) ++ "\n")

runTests :: Solution -> String -> String
runTests sln input
  | skipTests = "Tests skipped"
  | otherwise = (show $ sum $ (map (\x -> if x == () then 1 else 0) (map ($ input) (tests sln)))) ++ " passed"

