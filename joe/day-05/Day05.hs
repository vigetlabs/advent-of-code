module Day05 where
import Data.List (sort, group)

-- My setup is strange and I'm not happy about it. For now just copying
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitBy c s'' where (w, s'') = break (== c) s'

type Point = (Int, Int)
type Line = [Point]

exampleData :: String
exampleData = unlines [
    "0,9 -> 5,9"
  , "8,0 -> 0,8"
  , "9,4 -> 3,4"
  , "2,2 -> 2,1"
  , "7,0 -> 7,4"
  , "6,4 -> 2,0"
  , "0,9 -> 2,9"
  , "3,4 -> 1,4"
  , "0,0 -> 8,8"
  , "5,5 -> 8,2"]

parseLine :: String -> Line
parseLine str = let pair = fmap (fmap read . splitBy ',')
                            $ filter (/= "->")
                            $ words str in
  sort [(head $ head pair, last $ head pair),
   (head $ last pair, last $ last pair)]

solve1 :: [Line] -> Int
solve1 = length
  . filter (>= 2)
  . fmap length
  . group
  . sort
  . concatMap expandLine
  . filter isStraight

  where
    isStraight :: Line -> Bool
    isStraight [(x1,y1), (x2,y2)]
      | x1 == x2 = True
      | y1 == y2 = True
      | otherwise = False
    isStraight _ = error "Line must have 2 elements"

    expandLine :: Line -> [Point]
    expandLine [(x1, y1), (x2, y2)] = (,) <$> [x1..x2] <*> [y1..y2]
    expandLine _ = error "Line must have 2 elements"


main :: IO ()
main = do
  input <- readFile "day-05/input.txt"
  let ls = parseLine <$> lines input
  print $ solve1 ls
