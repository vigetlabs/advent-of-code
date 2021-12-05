module Day04 where
import Data.Either (fromRight)
import Data.List (transpose, inits, partition)

type Board = [[Int]]

-- Getting a decent split function was a PITA requiring either using Text or
-- pulling in a package. Pulling in a package didn't work with my editor config
-- for some reason, so I'm going to slum it and make my own.
-- Based on: https://stackoverflow.com/a/4981265
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitBy c s'' where (w, s'') = break (== c) s'

exampleData :: String
exampleData = unlines [
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
  , ""
  , "22 13 17 11  0"
  , "8  2 23  4 24"
  , "21  9 14 16  7"
  , "6 10  3 18  5"
  , "1 12 20 15 19"
  , ""
  , "3 15  0  2 22"
  , "9 18 13 17  5"
  , "19  8  7 25 23"
  , "20 11 10 24  4"
  , "14 21 16 12  6"
  , ""
  , "14 21 17 24  4"
  , "10 16 15  9 19"
  , "18  8 23 26 20"
  , "22 11 13  6  5"
  , "2  0 12  3  7"]

getBoards :: [[Int]] -> [Board]
getBoards [] = []
getBoards xs = take 5 xs:getBoards(drop 5 xs)

checkBoards :: Int -> Either ([Board], [Board]) Int -> [Int] -> Either ([Board], [Board]) Int
checkBoards _ (Right n) _ = Right n
checkBoards checkLen (Left (bs, compBoards)) ns =
  let (newCompleted, leftOver) = partition (checkBoard ns) bs
      totalComplete = newCompleted ++ compBoards in

  if length totalComplete == checkLen then
    Right $ (last ns *) $ sum $ filter (not . (`elem` ns)) $ concat $ head totalComplete
  else
    Left (leftOver, totalComplete)
  where
    checkBoard :: [Int] -> Board -> Bool
    checkBoard xs board = any (all (`elem` xs)) rowsAndColumns
      where rowsAndColumns = board ++ transpose board

solve1 :: [Int] -> [Board] -> Int
solve1 nums boards = fromRight 0 $ foldl (checkBoards 1) (Left (boards, [])) $ inits nums

solve2 :: [Int] -> [Board] -> Int
solve2 nums boards = fromRight 0 $ foldl (checkBoards $ length boards) (Left (boards, [])) $ inits nums

main :: IO ()
main = do
  input <- readFile "day-04/input.txt"
  let x:xs = filter ((/=) 0 . length) $ lines input
  let boards = getBoards $ fmap read <$> map words xs
  let nums = read <$> splitBy ',' x

  putStrLn "Part 1"
  print $ solve1 nums boards

  putStrLn "Part 2"
  print $ solve2 nums boards
