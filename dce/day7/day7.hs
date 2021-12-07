import System.IO

buildList :: String -> String -> [Int]
buildList "" s = read s : []
buildList (',' : cs) s = read s : buildList cs ""
buildList (c : cs) s = buildList cs (s ++ [c])

listMin :: [Int] -> Int
listMin is = foldl min (is !! 0) is

listMax :: [Int] -> Int
listMax is = foldl max (is !! 0) is

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let numbers = buildList input ""
    let options = [(listMin numbers)..(listMax numbers)]

    let cost newPos oldPos = abs (newPos - oldPos)
    let costs = map (\pos -> sum (map (cost pos) numbers)) options

    putStrLn ("Part 1: " ++ (show (listMin costs)))

    let cost newPos oldPos = sum [0..(abs (newPos - oldPos))]
    let costs = map (\pos -> sum (map (cost pos) numbers)) options

    putStrLn ("Part 2: " ++ (show (listMin costs)))
