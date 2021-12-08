import Data.List
import Data.Maybe
import System.IO

splitInput :: [String] -> [String] -> ([String], [String])
splitInput _ [] = (["what"], ["why"])
splitInput inputs ("|" : outputs) = (inputs, outputs)
splitInput inputs (s : ss) = splitInput (inputs ++ [s]) ss

identifiableDigit :: String -> Bool
identifiableDigit s =
    let len = length s in
    len == 2 || len == 3 || len == 4 || len == 7

segments :: [String] -> [String]
segments digits = 
    let len i s = length s == i in
    let matching fn = head (filter fn digits) in

    let one = matching (len 2) in
    let seven = matching (len 3) in
    let four = matching (len 4) in
    let eight = matching (len 7) in

    let three = matching (\s -> len 5 s && len 0 (one \\ s)) in
    let five = matching (\s -> len 5 s && s /= three && len 1 (four \\ s)) in
    let two = matching (\s -> len 5 s && s /= three && s /= five) in

    let nine = matching (\s -> len 6 s && len 0 (three \\ s)) in
    let zero = matching (\s -> len 6 s && s /= nine && len 0 (seven \\ s)) in
    let six = matching (\s -> len 6 s && s /= nine && s /= zero) in

    map sort [zero, one, two, three, four, five, six, seven, eight, nine]

valueForRow :: ([String], [String]) -> Int
valueForRow (inputs, outputs) =
    let segs = segments (inputs ++ outputs) in
    let digits = map (\s -> fromJust $ elemIndex (sort s) segs) outputs in
    foldl (\sum i -> sum * 10 + i) 0 digits

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let digits = map (splitInput [] . words) (lines input)
    let outputs = map snd digits

    let count = sum (map (length . filter identifiableDigit) outputs)

    putStrLn ("Part 1: " ++ (show count))

    let values = map valueForRow digits

    putStrLn ("Part 2: " ++ (show (sum values)))
