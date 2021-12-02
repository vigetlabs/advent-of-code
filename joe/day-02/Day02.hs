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

runCommand :: [Int] -> (String, Int) -> [Int]
runCommand [x, y] ("forward", n) = [x + n, y]
runCommand [x, y] ("down", n) = [x, y + n]
runCommand [x, y] ("up", n) = [x, y - n]
runCommand _ _ = error "invalid command"

solve :: [(String, Int)] -> Int
solve = product . foldl runCommand [0, 0]

parseLine :: String -> (String, Int)
parseLine l = (head $ words l, read $ words l !! 1)

main :: IO ()
main = do
  input <- readFile "day-02/input.txt"
  let xs = map parseLine $ lines input

  print $ solve xs

