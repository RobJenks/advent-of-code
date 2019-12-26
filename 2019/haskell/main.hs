import qualified Day1.Day1 as Day1
import qualified Day2.Day2 as Day2

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
  let solutions = [ Solution { day=1, part1=(io.Day1.part1), part2=(io.Day1.part2), tests=Day1.tests }
                  , Solution { day=2, part1=io.Day2.part1, part2=Day2.part2, tests=Day2.tests } ]
  
  runSolutions solutions 


runSolutions :: [Solution] -> IO()
runSolutions [] = do return ()
runSolutions (x:xs) = do
  runSolution x
  runSolutions xs
  

runSolution :: Solution -> IO()
runSolution solution = do
  input <- readFile ("Day" ++ (show $ day $ solution) ++ "/input")
  p1 <- (part1 solution $ input)
  p2 <- (part2 solution $ input)
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Tests : " ++ (runTests solution input))
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Part 1: " ++ p1)
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Part 2: " ++ p2)


runTests :: Solution -> String -> String
runTests sln input
  | skipTests = "Tests skipped"
  | otherwise = (show $ sum $ (map (\x -> if x == () then 1 else 0) (map ($ input) (tests sln)))) ++ " passed"

io :: a -> IO a
io x = do
 return x
