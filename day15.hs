matrixgetelement lst row = (!!) ((!!) lst row)

getelement matrix row col = head $ map (\(a, b, c) -> a) (filter (\(a, b, c) -> b == row && c == col) matrix)

-- взимане на елемент по подадена позиция

-- дали може да се движи наперд
canmover lst
  | head lst /= '@' = canmover (tail lst)
  | head lst == '@' && head (filter (\x -> x == '.' || x == '#') lst) == '.' = True
  | otherwise = False

-- функция която променя матрицата спрямо дали  движението е възможно
mover lst
  | canmover lst = takeWhile (/= '@') lst ++ ['.'] ++ takeWhile (/= '.') (dropWhile (/= '@') lst) ++ tail (dropWhile (/= '.') (dropWhile (/= '@') lst))
  | otherwise = lst

-- транслиране на матрица
tran matrix
  | null (head matrix) = []
  | otherwise = map head matrix : tran (map tail matrix)

-- една стъпка за движение
makemove lst
  | null $ head lst = []
  | '@' `elem` head lst = mover (head lst) : tail lst
  | otherwise = head lst : makemove (tail lst)

-- спрямо движението какво да направи матрицанта за да е обърната винаги ориентацията да се мести на дясно
iterat matrix dir
  | null dir = matrix
  | head dir == '>' = iterat (makemove matrix) (tail dir)
  | head dir == '<' = iterat (map reverse (makemove (map reverse matrix))) (tail dir)
  | head dir == '^' = iterat (tran $ map reverse (makemove (map reverse (tran matrix)))) (tail dir)
  | head dir == 'v' = iterat (tran (makemove (tran matrix))) (tail dir)

-- сметачки за крайния резултат
calrow row lst index
  | null lst = 0
  | head lst == 'O' = (100 * row) + index + calrow row (tail lst) (index + 1)
  | otherwise = calrow row (tail lst) (index + 1)

calcmatrix matrix index
  | null matrix = 0
  | otherwise = calrow index (head matrix) 0 + calcmatrix (tail matrix) (index + 1)

main = do
  content <- readFile "day15.txt"
  let inp = lines content
  let matrix = takeWhile (not . null) inp
  let dir = concat $ tail $ dropWhile (not . null) inp
  let make = iterat matrix dir
  let val = calcmatrix make 0

  -- print $ make
  print val
