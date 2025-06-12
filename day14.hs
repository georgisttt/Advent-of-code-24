allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1 fg
  | allexist lst1 ['0' .. '9'] = if fg then read lst1 else 0 - (read lst1)
  | otherwise = 0

makelst lst temp fg
  | null lst = [isallintegers temp fg]
  | head lst == '-' = makelst (tail lst) temp False
  | head lst == ',' = isallintegers temp fg : makelst (tail lst) [] True
  | otherwise = makelst (tail lst) (temp ++ [head lst]) fg

makeelem lst = makelst (tail (dropWhile (/= '=') (takeWhile (/= ' ') lst))) [] True

makevect lst = makelst (tail $ dropWhile (/= '=') $ tail $ dropWhile (/= '=') lst) [] True

elemwithvec lst = makeelem lst : [makevect lst]

-- до тук четене на входни данни
-- единственото по интересно е че за първи път  се сблъсках  и с четене на отрицателни числа от файл за каллендара

-- функция за итеприране на стъпките както е казано в условитео

makesteps lst index row colum
  | index == 100 = lst
  | otherwise = makesteps (map (\[[a, b], [c, d]] -> [[borders (a + c) row, borders (b + d) colum], [c, d]]) lst) (index + 1) row colum

--- функцията за положенията на роботите спрямо една кордината
borders index max
  | index < 0 = max + index
  | index >= max = mod index max
  | otherwise = index

-- функцията за големия пример с размери 101 103
-- ако някой иска да я палзва за други размери да ги промени
func lst = map head $ makesteps lst 0 101 103

-- брой елемеенти във всеки квадрант
counti3 lst row column = length $ filter (\[x, y] -> x < row && y < column) lst

counti4 lst row column = length $ filter (\[x, y] -> x > row && y < column) lst

counti2 lst row column = length $ filter (\[x, y] -> x > row && y > column) lst

counti1 lst row column = length $ filter (\[x, y] -> x < row && y > column) lst

funci lst row column = counti3 lst row column * counti1 lst row column * counti2 lst row column * counti4 lst row column

-- отново ако някой иска да я ползва с други параметри да ги промени
enterbig lst = funci (func lst) 50 51

main = do
  content <- readFile "day14.txt"
  print (enterbig $ map elemwithvec (lines content))

-- da opravia redove i koloni i e gotovo