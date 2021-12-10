module Day08 where

import Data.List (intersect)

-- My setup is strange and I'm not happy about it. For now just copying
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitBy c s'' where (w, s'') = break (== c) s'

exampleData :: String
exampleData = unlines [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
  , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
  , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
  , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
  , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
  , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
  , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
  , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
  , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
  , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]

data Signal a = Signal [a] [a] KnownNumbers deriving Show
data KnownNumbers = KnownNumbers {
    getOne :: String
  , getFour :: String
  , getSeven :: String
  , getEight :: String
} deriving Show

knownLengths :: [Int]
knownLengths = [2, 4, 3, 7]

prepare :: String -> [Signal String]
prepare input = toSignal . map words . splitBy '|' <$> lines input
  where
    toSignal [x,y] = Signal x y (getKnownNumbers $ x ++ y)
    toSignal _ = error "signal must have input and output"

    getKnownNumbers :: [String] -> KnownNumbers
    getKnownNumbers strings = KnownNumbers {
        getOne=head $ filter isOne strings
      , getFour=head $ filter isFour strings
      , getSeven=head $ filter isSeven strings
      , getEight=head $ filter isEight strings
    }

solve1 :: [Signal String] -> Int
solve1 = sum . map countDigits
  where
    countDigits (Signal _ xs _) = length
      $ filter (`elem` knownLengths)
      $ length <$> xs

isOne :: String -> Bool
isOne  [_,_] = True
isOne  _ = False

isTwo :: KnownNumbers -> String -> Bool
isTwo nums xs 
  | length xs == 5 = (== 2) . length $ getFour nums `intersect` xs
  | otherwise = False

isThree :: KnownNumbers -> String -> Bool
isThree nums xs 
  | length xs == 5 = (== 2) . length $ getOne nums `intersect` xs
  | otherwise = False

isFour :: String -> Bool
isFour [_,_,_,_] = True
isFour _ = False

isFive :: KnownNumbers -> String -> Bool
isFive nums xs 
  | length xs == 5 = 
    ((== 2) . length $ getSeven nums `intersect` xs)
    && ((== 3) . length $ getFour nums `intersect` xs)
  | otherwise = False

isSix :: KnownNumbers -> String -> Bool
isSix nums xs 
  | length xs == 6 = (== 1) . length $ getOne nums `intersect` xs
  | otherwise = False

isSeven :: String -> Bool
isSeven [_,_,_] = True
isSeven _ = False

isEight :: String -> Bool
isEight [_,_,_,_,_,_,_] = True
isEight _ = False

isNine :: KnownNumbers -> String -> Bool
isNine nums xs 
  | length xs == 6 = 
    ((== 2) . length $ getOne nums `intersect` xs)
    && ((== 4) . length $ getFour nums `intersect` xs)
  | otherwise = False

isZero :: KnownNumbers -> String -> Bool
isZero nums xs
  | length xs == 6 =
    ((== 2) . length $ getOne nums `intersect` xs)
    && ((== 3) . length $ getFour nums `intersect` xs)
  | otherwise = False

translate :: KnownNumbers -> String -> Char
translate nums s
  | isZero nums s = '0'
  | isOne s = '1'
  | isTwo nums s = '2'
  | isThree nums s = '3'
  | isFour s = '4'
  | isFive nums s = '5'
  | isSix nums s = '6'
  | isSeven s = '7'
  | isEight s = '8'
  | isNine nums s = '9'
  | otherwise = error $ "unkown digit" ++ s

solve2 :: [Signal String] -> Int
solve2 = sum . map decodeSignal
  where
    decodeSignal :: Signal String -> Int
    decodeSignal (Signal _ outputs nums) = read $ map (translate nums) outputs

main :: IO ()
main = do
  input <- readFile "day-08/input.txt"
  let signals = prepare input

  putStrLn "Part 1"
  print $ solve1 signals

  putStrLn "Part 2"
  print $ solve2 signals
