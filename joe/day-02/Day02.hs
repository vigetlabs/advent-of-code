module Day02 where

exampleData :: [(String, Int)]
exampleData = [
  ("forward", 5),
  ("down", 5),
  ("forward", 8),
  ("up", 3),
  ("down", 8),
  ("forward", 2)
  ]


solve1 :: [(String, Int)] -> Int
solve1 = product . foldl runCommand [0, 0]
  where
    runCommand :: [Int] -> (String, Int) -> [Int]
    runCommand [x, y] ("forward", n) = [x + n, y]
    runCommand [x, y] ("down", n) = [x, y + n]
    runCommand [x, y] ("up", n) = [x, y - n]
    runCommand _ _ = error "invalid command"

solve2 :: [(String, Int)] -> Int
solve2 = product . take 2 . foldl runCommand [0, 0, 0]
  where
    runCommand :: [Int] -> (String, Int) -> [Int]
    runCommand [x, y, z] ("forward", n) = [x + n, y + z * n, z]
    runCommand [x, y, z] ("down", n) = [x, y, z + n]
    runCommand [x, y, z] ("up", n) = [x, y, z - n]
    runCommand _ _ = error "invalid command"


parseLine :: String -> (String, Int)
parseLine l = (head $ words l, read $ words l !! 1)

main :: IO ()
main = do
  input <- readFile "day-02/input.txt"
  let xs = map parseLine $ lines input

  putStrLn "Part 1"
  print $ solve1 xs

  putStrLn "Part 2"
  print $ solve2 xs

