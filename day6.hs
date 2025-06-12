matrixgetelement lst row = (!!) ((!!) lst row)

-- >>> промяна на матрицата по подаден индекс
change matrix (x, y) = take x matrix ++ [take y p ++ ['x'] ++ drop (y + 1) p] ++ drop (x + 1) matrix
  where
    p = (!!) matrix x

-- идентични функции за посоките , затова коментари само на първата

-- ако сме завили на дясно
diasno matrix (x, y)
  -- провери дали сме в очертанията на матрицата ако не пресметни крайния резултат
  | y + 1 == length (head matrix) = calculate matrix
  -- ако елемента пред нас е пропаст , завий на правилната посока
  | matrixgetelement matrix x (y + 1) == '#' = nadoly matrix (x, y)
  -- иначе продължи в същата посока като промени елемента да е вече обходен
  | otherwise = diasno (change matrix (x, y)) (x, y + 1)

nadoly matrix (x, y)
  | x + 1 == length (head matrix) = calculate matrix
  | matrixgetelement matrix (x + 1) y == '#' = naliavo matrix (x, y)
  | otherwise = nadoly (change matrix (x, y)) (x + 1, y)

naliavo matrix (x, y)
  | y == 0 = calculate matrix
  | matrixgetelement matrix x (y - 1) == '#' = nagore matrix (x, y)
  | otherwise = naliavo (change matrix (x, y)) (x, y - 1)

nagore matrix (x, y)
  | x == 0 = calculate matrix
  | matrixgetelement matrix (x - 1) y == '#' = diasno matrix (x, y)
  | otherwise = nagore (change matrix (x, y)) (x - 1, y)

-- намиране на началния елемент заедно с неговата позиция
begin matrix index = if any (\x -> x /= '.' && x /= '#') ((!!) matrix index) then index else begin matrix (index + 1)

calcx matrix = begin matrix 0

column row index = if head row /= '.' && head row /= '#' then index else column (tail row) (index + 1)

-- дава елемента на началната позиция
calcpos matrix = (p, column ((!!) matrix p) 0)
  where
    p = calcx matrix

-- стартираща функция р защото не знаем първоначалната ориентация на елемента
start el matrix pos
  | el == '^' = nagore matrix pos
  | el == '>' = diasno matrix pos
  | el == '<' = naliavo matrix pos
  | otherwise = nadoly matrix pos

-- 1 + защото не броя елемента на границата в края за обходен
func matrix = 1 + start (matrixgetelement matrix x y) matrix (x, y)
  where
    (x, y) = calcpos matrix

-- пресмята броя на елементите x  в финалната матрица
calculate matrix = sum $ map (length . filter (== 'x')) matrix

main = do
  content <- readFile "day6.txt"
  -- функцията за елементите
  print (func $ lines content)
