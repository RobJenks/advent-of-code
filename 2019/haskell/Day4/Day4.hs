module Day4.Day4
( part1
, part2
, tests
)
where


part1 :: String -> String
part1 x = "Not completed"

part2 :: String -> String
part2 x = "Not completed"





-- Tests
tests = [test1]

test1 _ = assertEqual 4 4


assertEqual x exp = if (x == exp) then () else error ("Error: " ++ show x ++ " != " ++ show exp)

