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

newtype LanternFish = LanternFish Int deriving (Show, Eq)
data Counts a = Counts a a a a a a a a a deriving (Show, Foldable)

tick :: LanternFish -> [LanternFish]
tick (LanternFish 0) = [LanternFish 6, LanternFish 8]
tick (LanternFish n) = [LanternFish $ n - 1]

prepare :: String -> [LanternFish]
prepare s = LanternFish . read <$> splitBy ',' s

solve :: [LanternFish] -> [[LanternFish]]
solve fishes = fishes : solve (concatMap tick fishes)

prepare2 :: String -> Counts Int
prepare2 s = let sortedInputs = sort $ read <$> splitBy ',' s :: [Int] in
  Counts (length $ filter (== 0) sortedInputs)
         (length $ filter (== 1) sortedInputs)
         (length $ filter (== 2) sortedInputs)
         (length $ filter (== 3) sortedInputs)
         (length $ filter (== 4) sortedInputs)
         (length $ filter (== 5) sortedInputs)
         (length $ filter (== 6) sortedInputs)
         (length $ filter (== 7) sortedInputs)
         (length $ filter (== 8) sortedInputs)

solve2 :: Counts Int -> [Counts Int]
solve2 cs@(Counts a b c d e f g h i) = cs : solve2 (Counts b c d e f g (h+a) i a)

main :: IO ()
main = do
  input <-  readFile "day-06/input.txt"
  let fishes = prepare input
  print "Day 1"
  print $ length $ last $ take 81 $ solve fishes

  let counts = prepare2 input
  print "Day 2"
  print $ sum $ last $ take 257 $ solve2 counts
