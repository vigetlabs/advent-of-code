module Day03 where
import Data.Char (digitToInt)
import Data.List (maximumBy, transpose, group, sort)
import Data.Ord (comparing)

exampleData :: [String]
exampleData = [
    "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]

solve1 :: [String] -> Int
solve1 xs = toGamma gammaString * toEpsilon gammaString
  where
    gammaString :: String
    gammaString = concatMap (show . fst)
      $ maximumBy (comparing snd)
      . zip [(0::Int)..]
      . map length
      . group
      . sort
      <$> transpose xs

    toGamma :: String -> Int
    toGamma = foldl (\acc x -> acc * 2 + digitToInt x) 0

    toEpsilon :: String -> Int
    toEpsilon = foldl (\acc x -> acc * 2 + 1 - digitToInt x) 0

main :: IO ()
main = do
  input <- readFile "day-03/input.txt"
  print $ solve1 $ lines input
