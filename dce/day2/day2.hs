import System.IO

data Direction = Forward | Down | Up deriving Show

strToDir :: String -> Direction
strToDir "forward" = Forward
strToDir "down" = Down
strToDir "up" = Up

strToCommand :: String -> String -> (Direction, Int)
strToCommand dir (' ' : input) = (strToDir dir, read input)
strToCommand dir (i : input) = strToCommand (dir ++ [i]) input

part1 :: (Int, Int) -> (Direction, Int) -> (Int, Int)
part1 (pos, depth) (Forward, amt) = (pos + amt, depth)
part1 (pos, depth) (Down, amt) = (pos, depth + amt)
part1 (pos, depth) (Up, amt) = (pos, depth - amt)

part2 :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
part2 (pos, depth, aim) (Forward, amt) = (pos + amt, depth + amt * aim, aim)
part2 (pos, depth, aim) (Down, amt) = (pos, depth, aim + amt)
part2 (pos, depth, aim) (Up, amt) = (pos, depth, aim - amt)

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let commands = map (strToCommand "") (lines input)

    let (pos, depth) = foldl part1 (0, 0) commands
    putStrLn ("Part 1: " ++ show (pos * depth))

    let (pos, depth, _) = foldl part2 (0, 0, 0) commands
    putStrLn ("Part 2: " ++ show (pos * depth))
