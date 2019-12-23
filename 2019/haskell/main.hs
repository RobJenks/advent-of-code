import qualified Day1.Day1 as Day1

data Solution = Solution { day :: Int
                         , part1 :: (String -> String)
                         , part2 :: (String -> String)
                         }

main :: IO()
main = do
  let solutions = [ Solution { day=1, part1=Day1.part1, part2=Day1.part2 }
                  , Solution { day=1, part1=Day1.part1, part2=Day1.part2 } ]

  runSolutions solutions 

  

runSolutions :: [Solution] -> IO()
runSolutions [] = do return ()
runSolutions (x:xs) = do
  runSolution x
  runSolutions xs
  

runSolution :: Solution -> IO()
runSolution solution = do
  input <- readFile ("Day" ++ (show $ day $ solution) ++ "/input")
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Part 1: " ++ (part1 solution $ input))
  putStrLn ("Day " ++ (show $ day $ solution) ++ ", Part 2: " ++ (part2 solution $ input))

