allexist lst1 lst2 = lst1 == filter (\x -> x `elem` lst2) lst1

isallintegers lst1
  | allexist lst1 ['0' .. '9'] = read lst1
  | otherwise = 0

frominttoarr number
  | number == 0 = []
  | otherwise = frominttoarr (div number 10) ++ [mod number 10]

-- функции за вход

matrixgetelement lst row = (!!) ((!!) lst row)

qsort [] = []
qsort (x : xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

nob [] = []
nob [a] = [a]
nob (a : b : xs) = if a == b then nob (b : xs) else a : nob (b : xs)

-- помощни функции

-- генериране на всички елементи на матрицата
generateall matrix = [(x, y) | x <- [0 .. length matrix - 1], y <- [0 .. length (head matrix) - 1]]

-- филтриране да са равни на даден индекс
filtercorrect matrix lst index = filter (\(x, y) -> matrixgetelement matrix x y == index) lst

-- дали са в границите
filterinside matrix = filter (\(x, y) -> x >= 0 && y >= 0 && x < length matrix && y < length (head matrix))

-- генериране на съседни елементи
gen lst = concat [[(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)] | (x, y) <- lst]

good matrix lst elems
  | length elems == 1 = filtercorrect matrix (filterinside matrix lst) (head elems)
  | otherwise = good matrix (gen (filtercorrect matrix (filterinside matrix lst) (head elems))) (tail elems)

-- за вска последователност от елементи може да ми изчисли функцията

-- премахва дубликатите с еднакво начало
generat matrix = sum $ map (\x -> length $ nob $ qsort $ good matrix [x] ['0' .. '9']) (generateall matrix)

-- брои всичко
othergen matrix = length $ good matrix (generateall matrix) ['0' .. '9']

main = do
  content <- readFile "day10.txt"

  --  за първата задача
  print (generat $ lines content)

  -- за втората задача
  print $ othergen (lines content)
