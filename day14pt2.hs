allexist lst1 lst2 = lst1 == filter (\x -> x `elem` lst2) lst1

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

-- нищо интересно до тук

-- като решавах задачата се зачудих какво може да се търси и как се проверява за наличието на елха и то до кой индекс да гледам
-- стигнах до същесвуването на една вертикална и една хоризонтална линия с дължина поне 25

-- за по лесно търсене първо сортиране и след това обхождане

qsort [] = []
qsort (x : xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

consecti lst crmax maxi
  | length lst <= 1 = max crmax maxi
  | head lst == head (tail lst) = consecti (tail lst) (crmax + 1) maxi
  | otherwise = consecti (tail lst) 1 (max maxi crmax)

-- променена функция за итериране на елементите
-- горна граница 100000 ако някой не е сигурен за същесвуване да я намали
makesteps lst index row colum
  | index == 100000 = -1
  | consecti (qsort (map (tail . head) lst)) 0 0 >= 25 && consecti (qsort (map (head . head) lst)) 0 0 >= 25 = index
  | otherwise = makesteps (qsort (map (\[[a, b], [c, d]] -> [[borders (a + c) row, borders (b + d) colum], [c, d]]) lst)) (index + 1) row colum

borders index max
  | index < 0 = max + index
  | index >= max = mod index max
  | otherwise = index

func lst = makesteps lst 0 101 103

main = do
  content <- readFile "day14.txt"
  print (func $ map elemwithvec (lines content))
