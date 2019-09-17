module Main where
  import Control.Exception
  import qualified Day1 as Day1


  main = do
    run 1 Day1.run "input/day1"



  run :: Int -> (String -> (String, String)) -> String -> IO ()
  run n fn inputFile = do 
    contents <- readFile inputFile
    let (part1, part2) = fn contents
    putStrLn $ "Day " ++ show n ++ ", Part 1: " ++ part1
    putStrLn $ "Day " ++ show n ++ ", Part 2: " ++ part2 ++ "\n"
    


