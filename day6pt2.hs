matrixgetelement lst row = (!!) ((!!) lst row)

-- ще напиша до къде е същото като първа част

-- трик
-- промяната на посоките съм добавил макс дължина която ми казва дали робота е направил повече стъпки от големината на матрицата което води до цикъл

diasno matrix (x, y) max current
  | current > max = 0
  | y + 1 == length (head matrix) = 1
  | matrixgetelement matrix x (y + 1) == '#' = nadoly matrix (x, y) max current
  | otherwise = diasno matrix (x, y + 1) max (current + 1)

nadoly matrix (x, y) max current
  | current > max = 0
  | x + 1 == length (head matrix) = 1
  | matrixgetelement matrix (x + 1) y == '#' = naliavo matrix (x, y) max current
  | otherwise = nadoly matrix (x + 1, y) max (current + 1)

naliavo matrix (x, y) max current
  | current > max = 0
  | y == 0 = 1
  | matrixgetelement matrix x (y - 1) == '#' = nagore matrix (x, y) max current
  | otherwise = naliavo matrix (x, y - 1) max (current + 1)

nagore matrix (x, y) max current
  | current > max = 0
  | x == 0 = 1
  | matrixgetelement matrix (x - 1) y == '#' = diasno matrix (x, y) max current
  | otherwise = nagore matrix (x - 1, y) max (current + 1)

begin matrix index = if any (\x -> x /= '.' && x /= '#') ((!!) matrix index) then index else begin matrix (index + 1)

calcx matrix = begin matrix 0

column row index = if head row /= '.' && head row /= '#' then index else column (tail row) (index + 1)

calcpos matrix = (p, column ((!!) matrix p) 0)
  where
    p = calcx matrix

start el matrix pos
  -- оптимизация ще напиша долу каква е
  | null matrix = 1
  | el == '^' = nagore matrix pos (length matrix * length (head matrix)) 0
  | el == '>' = diasno matrix pos (length matrix * length (head matrix)) 0
  | el == '<' = naliavo matrix pos (length matrix * length (head matrix)) 0
  | otherwise = nadoly matrix pos (length matrix * length (head matrix)) 0

change matrix (x, y) = take x matrix ++ [take y p ++ ['#'] ++ drop (y + 1) p] ++ drop (x + 1) matrix
  where
    p = (!!) matrix x

-- край на същото

-- слага на непразнету муста да се оправи да е по ефективно

addelement matrix (x, y)
  | matrixgetelement matrix x y == '.' = change matrix (x, y)
  -- ако е невалиден се подава на функцията и веднага ми казва дали може или не
  | otherwise = []

-- за всеки елемент на матрицата ако го добавя става ли закикляне
func matrix = map (\(z, c) -> start (matrixgetelement matrix x y) (addelement matrix (z, c)) (x, y)) [(z, c) | z <- [0 .. length matrix - 1], c <- [0 .. (length $ head matrix) - 1]]
  where
    (x, y) = calcpos matrix

main = do
  content <- readFile "day6.txt"
  -- намири броя на търсените позиции
  print (length $ filter (== 0) $ func (lines content))


-- P.S около десет минути за големия входен пример 
