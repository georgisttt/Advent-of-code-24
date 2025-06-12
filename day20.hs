

-- добавяне на позиция в матрицата на елементите 
addpos lst column
  | null lst = []
  | otherwise = (head lst, column) : addpos (tail lst) (column + 1)

addrow lst row
  | null lst = []
  | otherwise = map (\(a, b) -> (a, row, b)) (head lst) : addrow (tail lst) (row + 1)

-- взимане на еелемент по подадени позиции
matrixgetelement lst row = (!!) ((!!) lst row)



-- функция за обхождание на матрицата
-- * по условие пътя от началото до края е единствен така че може да се имплементира всякаква функиция за обхождане на път
-- избрах тази защото вече я бях писал за друга задача и я преизползвам с промени подходящи за тази задача
-- единствен път -> единствен начин да го обходим от началото до края , тогава времето за което сме на дадена валидна клетка е уникално и определено 
bfs matrix used current  (a,b,c)  index
    | b == head current  && c == head (tail current )  = used  ++ [(index , [b,c])]
    | otherwise = bfs matrix  (used++ [(index, current) ])   (head $   filter (\[b,c] -> matrixgetelement matrix b c /= '#' && [b,c ] `notElem`map snd  used   )  (genneibors current) ) (a,b,c)  (index+1)




-- помощни функции за работе на bfs  алгоритъма

-- проверка дали елемент е в матрицата
inborders el  row column = head el >=0 && head el < row && (!! ) el 1 >=0 && (!!)  el 1 <column

-- генериране на съседни елементи на клетка 
genneibors [x,y] =  [[x,y+1 ] , [x,y-1] , [x-1 ,y] ,[x+1,y]   ]



-- обединение на множества 
remsec temp lst
  | null temp = []
  | head temp `elem` lst = remsec (tail temp) lst
  | otherwise = head temp : remsec (tail temp) lst


-- премахване на повтарящи съседни елементи 
remdups lst
  | null lst = []
  | length lst == 1 = lst
  | head lst == head (tail lst) = remdups (tail lst)
  | otherwise = head lst : remdups (tail lst)








-- сортировач
qsort [] =[ ]
qsort  (x:xs)= qsort [y| y<-xs , y <=x] ++[x] ++  qsort [y| y<-xs , y >x]



-- дали множество е същинско подмножество на другото 
allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

-- на всяка позиция трябва да проверим след две секунди дали сме съкратили пътя 
-- прескачаме две полета
generateallpos [a,b ] = [[a+x , b+y ]| x<- [-2.. 2 ] , y <- [-2 .. 2  ] , abs x + abs y ==2 ]



-- провека дали сме на валидна позиция след скока в матрицата
isgood  [a,b]   matrix = inborders [a,b]  (length matrix) (length $ head matrix ) && matrixgetelement matrix a b /= '#'




-- главна функция която за всеки елемент на пътя връща коректните елементи
generategood  lst  matrix = map ((filter (`isgood` matrix) . generateallpos) . snd) lst




-- полезни функции за обработка на типовете ми
-- намиране на прескочената позиция кога се е появила в обходения път 
findsec path el =  fst $ head (filter (\x -> snd x == el )   path )

fori  lst path = map  (map (findsec  path  ) ) lst



-- последователо обхождане на елементите във всяко множество
-- на готово ползвам че за списък на x позиция е достигнато за време x
iteratethem lst index
    |null lst =[]
    |otherwise = map (\x -> x-index -2) (head lst ) ++ iteratethem (tail lst ) (index +1)

















main = do
  content <- readFile "day20.txt"
  let ff = lines content
  let beg = concat (addrow (map (`addpos` 0) (lines content)) 0)
  let used = map (\(a, b, c) -> [b, c]) (filter (\(x, y, z) -> x == '#') beg)
  let start =head $  map (\(a, b, c) -> [b, c]) (filter (\(x, y, z) -> x == 'S') beg)
  let end  = head $  filter  (\(x,y,z) -> x== 'E') beg
  let path =  bfs ff [] start   end  0
  let goodi =  generategood  path   ff
  let finale = fori goodi path

-- филтъра  е по желание на условие на задачата 
-- за гевнерирането на всички скокове ако се иска да се махне
  let finale2 = filter (>= 100)  ( iteratethem finale 0)

  -- за да визуализира бройката и че се прави нещо 
  -- при нежелание от нея да се истрие долния ред 
  print finale2
  print (length finale2)
