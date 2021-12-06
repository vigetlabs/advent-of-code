import System.IO

buildList :: String -> String -> [Int]
buildList "" s = read s : []
buildList (',' : cs) s = read s : buildList cs ""
buildList (c : cs) s = buildList cs (s ++ [c])

emptyList :: [Int]
emptyList = map (\_ -> 0) [0..8]

incAt :: [Int] -> Int -> Int -> [Int]
incAt (i : is) 0 amt = i + amt : is
incAt (i : is) idx amt = i : incAt is (idx - 1) amt

iter :: [Int] -> [Int]
iter fish = 
    let inc f n = if n == 0
                      then incAt (incAt f 8 (fish !! 0)) 6 (fish !! 0)
                  else incAt f (n - 1) (fish !! n) in
    foldl inc emptyList [0..8]

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let numbers = buildList input ""
    let list = foldl (\f i -> incAt f i 1) emptyList numbers

    let result = foldl (\s _ -> iter s) list [0..79]
    let sum = foldl (+) 0 result

    putStrLn ("Part 1: " ++ (show sum))

    let result = foldl (\s _ -> iter s) list [0..255]
    let sum = foldl (+) 0 result

    putStrLn ("Part 2: " ++ (show sum))
