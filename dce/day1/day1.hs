import System.IO

part1 :: (Int, Int) -> Int -> (Int, Int)
part1 (count, prev) depth =
   if depth > prev
       then (count + 1, depth)
   else (count, depth)

part2 :: (Int, [Int]) -> Int -> (Int, [Int])
part2 (count, prev) depth =
   if length prev < 3
      then (count, [depth] ++ prev)
   else
      let newPrev = [depth] ++ take 2 prev in

      if sum newPrev > sum prev
         then (count + 1, newPrev)
      else (count, newPrev)

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let depths = map read (lines input)

    let (count, _) = foldl part1 (0, 10000) depths
    putStrLn ("Part 1: " ++ show count)

    let (count, _) = foldl part2 (0, []) depths
    putStrLn ("Part 2: " ++ show count)
