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

tick :: LanternFish -> [LanternFish]
tick (LanternFish 0) = [LanternFish 6, LanternFish 8]
tick (LanternFish n) = [LanternFish $ n - 1]

prepare :: String -> [LanternFish]
prepare s = LanternFish . read <$> splitBy ',' s

solve :: [LanternFish] -> [[LanternFish]]
solve fishes = fishes : solve (concatMap tick fishes)

main :: IO ()
main = do
  input <-  readFile "day-06/input.txt"
  let fishes = prepare input
  print "Day 1"
  print $ length $ last $ take 81 $ solve fishes
