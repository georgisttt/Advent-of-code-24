allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1
  | allexist lst1 ['0' .. '9'] = read lst1
  | otherwise = 0

numberswithseparator lst temp
  | null lst = [isallintegers temp]
  | head lst == ' ' = [isallintegers temp] ++ numberswithseparator (tail lst) []
  | otherwise = numberswithseparator (tail lst) (temp ++ [head lst])

-- четене на вход

-- функция за определяне на растене или намаляване

allin lst temp
  | null lst = True
  | ((head lst) - temp < 1) || ((head lst) - temp > 3) = False
  | otherwise = allin (tail lst) (head lst)

-- >>>allincdic  [7, 6, 4, 2 ,1]
-- True

alldic lst temp
  | null lst = True
  | (head lst - temp < (-3)) || (head lst - temp > (-1)) = False
  | otherwise = alldic (tail lst) (head lst)

allincdic lst = (alldic (tail lst) (head lst)) || (allin (tail lst) (head lst))

main = do
  content <- readFile "day2.txt"

  let inp = map (\x -> numberswithseparator x []) (lines content)

  print $ length $ filter id $ map allincdic inp
