module Day07 where

-- My setup is strange and I'm not happy about it. For now just copying
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitBy c s'' where (w, s'') = break (== c) s'

exampleData :: String
exampleData = "16,1,2,0,4,2,7,1,2,14"

data CrabMarine = CrabMarine { getPosition :: Int, getFuel :: Int } deriving (Eq, Ord, Show)

prepare :: String -> [CrabMarine]
prepare input = map crabMarine $ read <$> splitBy ',' input
  where
    crabMarine x = CrabMarine { getPosition=x, getFuel=0 }

solve1 :: [CrabMarine] -> Int
solve1 crabMarines = minimum
   $ (sum . map getFuel) . moveMarines
  <$> [minPos..maxPos]
  where
    moveMarines x = map (\f -> f x) $ moveTo <$> crabMarines
    (minPos, maxPos) = (,) <$> minimum <*> maximum $ map getPosition crabMarines
    moveTo :: CrabMarine -> Int -> CrabMarine
    moveTo CrabMarine {
      getPosition = pos
    } n = CrabMarine { getPosition=n, getFuel=abs $ n - pos }

solve2 :: [CrabMarine] -> Int
solve2 crabMarines = minimum
   $ (sum . map getFuel) . moveMarines
  <$> [minPos..maxPos]
  where
    moveMarines x = map (\f -> f x) $ moveTo <$> crabMarines
    (minPos, maxPos) = (,) <$> minimum <*> maximum $ map getPosition crabMarines
    moveTo :: CrabMarine -> Int -> CrabMarine
    moveTo CrabMarine {
        getPosition = pos
    } n = CrabMarine { getPosition=n, getFuel=fuelForDistance $ abs $ n - pos }

    fuelForDistance 0 = 0
    fuelForDistance n = n + fuelForDistance (n - 1)

main :: IO ()
main = do
  input <- readFile "day-07/input.txt"
  let crabMarines = prepare input
  putStrLn  "Part 1"
  print $ solve1 crabMarines

  putStrLn  "Part 2"
  print $ solve2 crabMarines
