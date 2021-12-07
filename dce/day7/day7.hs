import System.IO

buildList :: String -> String -> [Int]
buildList "" s = read s : []
buildList (',' : cs) s = read s : buildList cs ""
buildList (c : cs) s = buildList cs (s ++ [c])

minCost :: (Int -> Int -> Int) -> [Int] -> Int
minCost costFn positions =
    let options = [(minimum positions)..(maximum positions)] in
    let costs = map (\p -> sum (map (costFn p) positions)) options in
    minimum costs

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let positions = buildList input ""

    let cost = minCost (\p1 p2 -> abs (p1 - p2)) positions

    putStrLn ("Part 1: " ++ (show cost))

    let cost = minCost (\p1 p2 -> sum [0..(abs (p1 - p2))]) positions

    putStrLn ("Part 2: " ++ (show cost))
