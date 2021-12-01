module Day01 where

import Data.List (tails)

windows :: Int -> [Int] -> [[Int]]
windows x = filter ((x ==) . length) . map (take x) . tails

solve1 :: [Int] -> Int
solve1 = length . filter isGreater . windows 2
  where
    isGreater [x, y] = y > x
    isGreater _ = error "wrong sized window"

solve2 :: [Int] -> Int
solve2 = solve1 . map sum . windows 3

main :: IO ()
main = do
  input <- readFile "day-01/input.txt"
  let xs = map read $ lines input

  putStrLn "Part 1"
  print $ solve1 xs

  putStrLn "Part 2"
  print $ solve2 xs
