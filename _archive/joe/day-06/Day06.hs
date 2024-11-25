{-# LANGUAGE DeriveFoldable #-}
module Day06 where
import Data.List (sort)

-- My setup is strange and I'm not happy about it. For now just copying
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitBy c s'' where (w, s'') = break (== c) s'

exampleData :: String
exampleData = "3,4,3,1,2"

data Counts a = Counts a a a a a a a a a deriving (Show, Foldable)

prepare :: String -> Counts Int
prepare s = let sortedInputs = sort $ read <$> splitBy ',' s :: [Int] in
  Counts (length $ filter (== 0) sortedInputs)
         (length $ filter (== 1) sortedInputs)
         (length $ filter (== 2) sortedInputs)
         (length $ filter (== 3) sortedInputs)
         (length $ filter (== 4) sortedInputs)
         (length $ filter (== 5) sortedInputs)
         (length $ filter (== 6) sortedInputs)
         (length $ filter (== 7) sortedInputs)
         (length $ filter (== 8) sortedInputs)

solve :: Counts Int -> [Counts Int]
solve cs@(Counts a b c d e f g h i) = cs : solve (Counts b c d e f g (h+a) i a)

main :: IO ()
main = do
  input <-  readFile "day-06/input.txt"
  let counts = prepare input
  print "Day 1"
  print $ sum $ (!! 80) $ solve counts

  print "Day 2"
  print $ sum $ (!! 256) $ solve counts
