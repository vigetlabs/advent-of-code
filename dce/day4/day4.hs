import System.IO

readInt :: String -> Int
readInt str = read str :: Int

splitOnCommas :: String -> String -> [String] -> [String]
splitOnCommas "" word list = list ++ [word]
splitOnCommas (',' : input) word list = splitOnCommas input "" (list ++ [word])
splitOnCommas (i : input) word list = splitOnCommas input (word ++ [i]) list

readRow :: String -> [(Int, Bool)]
readRow input =
    let numbers = map read (words input) :: Int in
    map (\n -> (n, False)) numbers

readBoard :: [String] -> [[(Int, Bool)]] -> [[(Int, Bool)]]
readBoard [] board = board
readBoard (row : rows) board = readBoard rows (board ++ [readRow row])

readBoards :: [String] -> [[[(Int, Bool)]]] -> [[[(Int, Bool)]]]
readBoards [] boards = boards
readBoards inputs boards =
    let board = readBoard (drop 1 (take 6 inputs)) [] in
    readBoards (drop 6 inputs) (boards ++ [board])

applyNumberToRow :: Int -> [(Int, Bool)] -> [(Int, Bool)]
applyNumberToRow number row =
    let check = \(n, p) -> if n == number then (n, True) else (n, p) in
    map check row

applyNumberToBoard :: Int -> [[(Int, Bool)]] -> [[(Int, Bool)]]
applyNumberToBoard n = map (applyNumberToRow n)

rowComplete :: [(Int, Bool)] -> Bool
rowComplete row =
    let check = \complete (_, checked) -> complete && checked in
    foldl check True row

boardCols :: [[(Int, Bool)]] -> [[(Int, Bool)]]
boardCols board =
    map (\i -> map (!! i) board) [0..(length (board !! 0) - 1)]

boardComplete :: [[(Int, Bool)]] -> Bool
boardComplete board =
    let anyComplete = \c r -> c || rowComplete r in
    let completeRow = foldl anyComplete False board in
    let completeCol = foldl anyComplete False (boardCols board) in
    completeRow || completeCol

getWinner :: [Int] -> [[[(Int, Bool)]]] -> ([[(Int, Bool)]], Int)
getWinner (n : ns) boards =
    let updatedBoards = map (applyNumberToBoard n) boards in
    let winners = filter boardComplete updatedBoards in
    if length winners > 0
        then (winners !! 0, n)
    else getWinner ns updatedBoards

boardScore :: [[(Int, Bool)]] -> Int
boardScore board =
    let spaceScore = \(i, c) -> if c then 0 else i in
    let rowScore = foldl (\sc sp -> sc + spaceScore sp) 0 in
    let rowScores = map rowScore board in
    foldl (\sc r -> sc + r) 0 rowScores

getLastWinner :: [Int] -> [[[(Int, Bool)]]] -> ([[(Int, Bool)]], Int)
getLastWinner (n : ns) boards =
    let updatedBoards = map (applyNumberToBoard n) boards in
    let winners = filter boardComplete updatedBoards in
    let losers = filter (not . boardComplete) updatedBoards in
    if length boards == 1 && length winners == 1
        then (winners !! 0, n)
    else getLastWinner ns losers

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let (l : ls) = lines input
    let numbers = map readInt (splitOnCommas l "" [])
    let boards = readBoards ls []

    let (board, number) = getWinner numbers boards
    let score = boardScore board

    putStrLn ("Part 1: " ++ show (score * number))

    let (board, number) = getLastWinner numbers boards
    let score = boardScore board

    putStrLn ("Part 2: " ++ show (score * number))
