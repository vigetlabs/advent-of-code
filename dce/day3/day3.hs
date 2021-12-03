import Data.Char
import System.IO

bitsToDec :: [Int] -> Int
bitsToDec [i] = i
bitsToDec (i : is) = (bitsToDec is * 2) + i

bitCount :: Int -> (Int, Int) -> [Int] -> (Int, Int)
bitCount place (zeroes, ones) input =
    if input !! place == 0
        then (zeroes + 1, ones)
    else (zeroes, ones + 1)

moreCommonBit :: [[Int]] -> Int -> Int
moreCommonBit inputs place =
    let (zeroes, ones) = foldl (bitCount place) (0, 0) inputs in
    if zeroes > ones
       then 0
    else 1 

lessCommonBit :: [[Int]] -> Int -> Int
lessCommonBit inputs place =
    let (zeroes, ones) = foldl (bitCount place) (0, 0) inputs in
    if zeroes > ones
       then 1
    else 0

filterOxy :: [[Int]] -> Int -> [[Int]]
filterOxy [input] _ = [input]
filterOxy inputs place =
    let bit = moreCommonBit inputs place in
    filter (\s -> (s !! place) == bit) inputs

filterCo2 :: [[Int]] -> Int -> [[Int]]
filterCo2 [input] _ = [input]
filterCo2 inputs place =
    let bit = lessCommonBit inputs place in
    filter (\s -> (s !! place) == bit) inputs

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let inputs = map (map digitToInt) (lines input)
    let places = [0..(length (inputs !! 0) - 1)]

    let gammaBits = map (moreCommonBit inputs) places
    let gamma = bitsToDec (reverse gammaBits)

    let epsilonBits = map (lessCommonBit inputs) places
    let epsilon = bitsToDec (reverse epsilonBits)

    putStrLn ("Part 1: " ++ (show (gamma * epsilon)))

    let oxyBits = (foldl filterOxy inputs places) !! 0
    let oxy = bitsToDec (reverse oxyBits)

    let co2Bits = (foldl filterCo2 inputs places) !! 0
    let co2 = bitsToDec (reverse co2Bits)

    putStrLn ("Part 2: " ++ (show (oxy * co2)))
