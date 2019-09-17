module Day1 where

  run :: String -> (String, String)
  run input = (part1 input, part2 input)


  part1 :: String -> String
  part1 input = show $ foldr (\x acc -> acc + if x == '(' then 1 else (-1)) 0 input 

  part2 :: String -> String
  part2 input = show $ length $ takeWhile (>=0)
                              $ scanl (\acc x -> acc + if x == '(' then 1 else (-1)) 0 input

