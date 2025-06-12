

-- идея
-- не прави нищо различно само добави колко време е продължил скока
-- и промени изчислението на границата
-- не много надейно решение защото отнема 1-2 часа за намиране на крайния резултат
-- извинявам се това е единствената функция с толкова бавно изпълнение
-- не хубава идея 



addpos lst column
  | null lst = []
  | otherwise = (head lst, column) : addpos (tail lst) (column + 1)

addrow lst row
  | null lst = []
  | otherwise = map (\(a, b) -> (a, row, b)) (head lst) : addrow (tail lst) (row + 1)



bfs matrix used current  (a,b,c)  index
    | b == head current  && c == head (tail current )  = used  ++ [(index , [b,c])]
    | otherwise = bfs matrix  (used++ [(index, current) ])   (head $   filter (\[b,c] -> matrixgetelement matrix b c /= '#' && [b,c ] `notElem`map snd  used   )  (genneibors current) ) (a,b,c)  (index+1)




matrixgetelement lst row = (!!) ((!!) lst row)

genneibors [x,y] =  [[x,y+1 ] , [x,y-1] , [x-1 ,y] ,[x+1,y]   ]

remsec temp lst
  | null temp = []
  | head temp `elem` lst = remsec (tail temp) lst
  | otherwise = head temp : remsec (tail temp) lst

remdups lst
  | null lst = []
  | length lst == 1 = lst
  | head lst == head (tail lst) = remdups (tail lst)
  | otherwise = head lst : remdups (tail lst)


inborders el  row column = head el >=0 && head el < row && (!! ) el 1 >=0 && (!!)  el 1 <column

qsort [] =[ ]
qsort  (x:xs)= qsort [y| y<-xs , y <=x] ++[x] ++  qsort [y| y<-xs , y >x]




allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isgood  [a,b]   matrix = inborders [a,b]  (length matrix) (length $ head matrix ) && matrixgetelement matrix a b /= '#'


-- нищо ново до тук  

-- тук вече е плюс двайсет заради условиеето скока да е с макс 20 манхатанско разстояние
-- гарантира уникалност с това че само един път добавяме всеки един "възможен елемент "
generateallpos [a,b ] = [([a+x , b+y  ] , abs x + abs y  )| x<- [-20.. 20 ] , y <- [-20 .. 20  ] , abs x + abs y <=20 ]




--генериране на всички възмогжни по групи 
generategood  lst  matrix =    map ((filter (\x -> isgood  (fst x)  matrix ) . generateallpos) . snd) lst



findsec path el =  fst $ head (filter (\x -> snd x == el )   path )

fori  lst path leni = map  (map (\(  a ,b ) -> ( findsec  path a, b)  ) ) lst


-- променена съкратена функция за намиране на перскоченото разстояние
-- изплолзва се същия трик
iteratethem lst index
    |null lst =[]
    |otherwise = map (\(a,b ) ->  a-b-index   ) (head lst ) : iteratethem (tail lst ) (index +1)

















main = do
  content <- readFile "day20.txt"
  let ff = lines content
  let beg = concat (addrow (map (`addpos` 0) (lines content)) 0)
  let used = map (\(a, b, c) -> [b, c]) (filter (\(x, y, z) -> x == '#') beg)
  let start =head $  map (\(a, b, c) -> [b, c]) (filter (\(x, y, z) -> x == 'S') beg)
  let end  = head $  filter  (\(x,y,z) -> x== 'E') beg
  let path =  bfs ff [] start   end  0

  let goodi =  generategood  path   ff
  let z =length path
  let finale = fori goodi path z
  let finale2 = iteratethem finale 0

  let c = map qsort finale
  let r =  map (\x -> z -x ) $  concat  finale2

-- 100 заради условие
  let ddd =  concatMap (filter (>= 100)) finale2
  --за визуализация
  print ddd
  -- търсения отговор 
  print $ length ddd
