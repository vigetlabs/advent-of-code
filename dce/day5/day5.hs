import System.IO

readPoint :: String -> String -> (Int, Int)
readPoint (',' : y) x = (read x, read y)
readPoint (c : cs) x = readPoint cs (x ++ [c])

readVector :: String -> ((Int, Int), (Int, Int))
readVector input =
    let strs = words input in
    let p1 = readPoint (strs !! 0) "" in
    let p2 = readPoint (strs !! 2) "" in
    (p1, p2)

sortVector :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
sortVector ((x1, y1), (x2, y2)) =
    if y2 < y1 || (y1 == y2 && x2 < x1)
       then ((x2, y2), (x1, y1))
    else
      ((x1, y1), (x2, y2))

horizOrVert :: ((Int, Int), (Int, Int)) -> Bool
horizOrVert ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

applyVectorToRow :: [Int] -> ((Int, Int), (Int, Int)) -> [Int]
applyVectorToRow [] ((0, 0), (0, _)) = [1]
applyVectorToRow [] ((0, 0), (x2, y2)) =
    1 : applyVectorToRow [] ((0, 0), (x2 - 1, y2))
applyVectorToRow [] ((0, _), (_, _)) = []
applyVectorToRow [] ((x1, y1), (x2, y2)) =
    0 : applyVectorToRow [] ((x1 - 1, y1), (x2 - 1, y2))
applyVectorToRow (i : is) ((0, 0), (0, _)) = i + 1 : is
applyVectorToRow (i : is) ((0, 0), (x2, y2)) =
    i + 1 : applyVectorToRow is ((0, 0), (x2 - 1, y2))
applyVectorToRow (i : is) ((0, _), (_, _)) = is
applyVectorToRow (i : is) ((x1, y1), (x2, y2)) =
    i : applyVectorToRow is ((x1 - 1, y1), (x2 - 1, y2))

applyVectorToMap :: [[Int]] -> ((Int, Int), (Int, Int)) -> [[Int]]
applyVectorToMap [] ((x1, 0), (x2, 0)) =
    [applyVectorToRow [] ((x1, 0), (x2, 0))]
applyVectorToMap [] ((x1, 0), (x2, y2)) =
    let row = applyVectorToRow [] ((x1, 0), (x2, 0)) in
    row : applyVectorToMap [] ((x1, 0), (x2, y2 - 1))
applyVectorToMap [] ((x1, y1), (x2, y2)) =
    [] : applyVectorToMap [] ((x1, y1 - 1), (x2, y2 - 1))
applyVectorToMap (r : rs) ((x1, 0), (x2, 0)) =
    applyVectorToRow r ((x1, 0), (x2, 0)) : rs
applyVectorToMap (r : rs) ((x1, 0), (x2, y2)) =
    let row = applyVectorToRow r ((x1, 0), (x2, 0)) in
    row : applyVectorToMap rs ((x1, 0), (x2, y2 - 1))
applyVectorToMap (r : rs) ((x1, y1), (x2, y2)) =
    r : applyVectorToMap rs ((x1, y1 - 1), (x2, y2 - 1))

incRowAt :: [Int] -> Int -> [Int]
incRowAt [] 0 = [1]
incRowAt [] idx = 0 : incRowAt [] (idx - 1)
incRowAt (i : is) 0 = i + 1 : is
incRowAt (i : is) idx = i : incRowAt is (idx - 1)

applyDiagToMap :: [[Int]] -> ((Int, Int), (Int, Int)) -> [[Int]]
applyDiagToMap [] ((x1, 0), (x2, 0)) =
    [incRowAt [] x1]
applyDiagToMap [] ((x1, 0), (x2, y2)) =
    let row = incRowAt [] x1 in
    row : if x1 > x2
             then applyDiagToMap [] ((x1 - 1, 0), (x2 - 1, y2 - 1))
          else applyDiagToMap [] ((x1 + 1, 0), (x2 + 1, y2 - 1))
applyDiagToMap [] ((x1, y1), (x2, y2)) =
    [] : applyDiagToMap [] ((x1, y1 - 1), (x2, y2 - 1))
applyDiagToMap (r : rs) ((x1, 0), (x2, 0)) =
    incRowAt r x1 : rs
applyDiagToMap (r : rs) ((x1, 0), (x2, y2)) =
    let row = incRowAt r x1 in
    row : if x1 > x2
             then applyDiagToMap rs ((x1 - 1, 0), (x2 - 1, y2 - 1))
          else applyDiagToMap rs ((x1 + 1, 0), (x2 + 1, y2 - 1))
applyDiagToMap (r : rs) ((x1, y1), (x2, y2)) =
    r : applyDiagToMap rs ((x1, y1 - 1), (x2, y2 - 1))

mapScore :: [[Int]] -> Int
mapScore m =
  let rowScore = length . filter (> 1) in
  let rowScores = map rowScore m in
  foldl (+) 0 rowScores

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let vectors = map (sortVector . readVector) (lines input)
    let nonDiagonalVectors = filter horizOrVert vectors

    let board = foldl applyVectorToMap [] nonDiagonalVectors
    let ms = mapScore board

    putStrLn ("Part 1: " ++ (show ms))

    let board = foldl (\m v -> if horizOrVert v
                                  then applyVectorToMap m v
                               else applyDiagToMap m v) [] vectors
    let ms = mapScore board

    putStrLn ("Part 2: " ++ (show ms))
