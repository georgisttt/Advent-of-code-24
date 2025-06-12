allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1
  | allexist lst1 ['0' .. '9'] = read lst1
  | otherwise = 0

numberswithseparator lst
  | notElem ' ' lst = []
  | otherwise = [(isallintegers (takeWhile (/= ' ') lst), isallintegers (dropWhile (== ' ') (dropWhile (/= ' ') lst)))]

-- функции за четене на вход




-- функцията за финалния резултят на 1 подточка

findres lst =sum $  zipWith (\x y -> abs (x-y)) (qsort $ map fst lst )  (qsort $ map snd lst )


qsort []=[]
qsort (x:xs) = qsort [y| y <- xs , y<=x] ++[x] ++ qsort [y| y <- xs , y>x]


-- функцията за финалния резултят на 2 подточка
findsimilar lst1 lst2 =sum $ zipWith (* ) ( map   (\x -> length (filter  (== x) lst2))   lst1) lst1


main = do
  content <- readFile "day1.txt"
  let inp =  concatMap numberswithseparator (lines content)

-- за 1 подточка
  print  $ findres inp

-- за втора подточка
  print $ findsimilar (map fst inp) (map snd inp  )
