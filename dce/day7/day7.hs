import System.IO

buildList :: String -> String -> [Int]
buildList "" s = read s : []
buildList (',' : cs) s = read s : buildList cs ""
buildList (c : cs) s = buildList cs (s ++ [c])

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let numbers = buildList input ""
    let options = [(minimum numbers)..(maximum numbers)]

    let cost newPos oldPos = abs (newPos - oldPos)
    let costs = map (\pos -> sum (map (cost pos) numbers)) options

    putStrLn ("Part 1: " ++ (show (minimum costs)))

    let cost newPos oldPos = sum [0..(abs (newPos - oldPos))]
    let costs = map (\pos -> sum (map (cost pos) numbers)) options

    putStrLn ("Part 2: " ++ (show (minimum costs)))
