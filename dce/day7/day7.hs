import System.IO

buildList :: String -> String -> [Int]
buildList "" s = read s : []
buildList (',' : cs) s = read s : buildList cs ""
buildList (c : cs) s = buildList cs (s ++ [c])

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let positions = buildList input ""
    let options = [(minimum positions)..(maximum positions)]

    let cost pos1 pos2 = abs (pos1 - pos2)
    let costs = map (\p -> sum (map (cost p) positions)) options

    putStrLn ("Part 1: " ++ (show (minimum costs)))

    let cost pos1 pos2 = sum [0..(abs (pos1 - pos2))]
    let costs = map (\p -> sum (map (cost p) positions)) options

    putStrLn ("Part 2: " ++ (show (minimum costs)))
