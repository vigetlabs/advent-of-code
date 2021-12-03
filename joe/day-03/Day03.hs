module Day03 where
import Data.Char (digitToInt)
import Data.List (minimumBy, maximumBy, transpose, group, sort, sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Bifunctor (second)

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

toBinary :: [Int] -> Int
toBinary = foldl (\acc x -> acc * 2 + x) 0

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
    toGamma = toBinary . map digitToInt

    toEpsilon :: String -> Int
    toEpsilon = toBinary . map ((1 -) . digitToInt)

solve2 :: [String] -> Int
solve2 xs = oxygenRating (zip xs xs) * co2Rating (zip xs xs)
  where
    oxygenRating :: [(String, String)] -> Int
    oxygenRating [] = error "no input"
    oxygenRating [(y, _)] = toBinary $ map digitToInt y
    oxygenRating ys = oxygenRating
      $ map (second tail)
      $ maximumBy (compare `on` length)
      $ groupBy ((==) `on` head . snd)
      $ sortBy (compare `on` snd) ys

    co2Rating :: [(String, String)] -> Int
    co2Rating [] = error "no input"
    co2Rating [(y, _)] = toBinary $ map digitToInt y
    co2Rating ys = co2Rating
      $ map (second tail)
      $ minimumBy (compare `on` length)
      $ groupBy ((==) `on` head . snd)
      $ sortBy (compare `on` snd) ys


main :: IO ()
main = do
  input <- readFile "day-03/input.txt"

  putStrLn "Part 1"
  print $ solve1 $ lines input

  putStrLn "Part 2"
  print $ solve2 $ lines input
