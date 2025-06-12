
allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1 fg
  | allexist lst1 ['0' .. '9'] = if fg then read lst1 else 0 - (read lst1)
  | otherwise = 0
makelst lst temp fg
  | null lst = [isallintegers temp fg]
  | head lst == '-' = makelst (tail lst) temp False
  | head lst == ',' = isallintegers temp fg : makelst (tail lst) [] True
  | otherwise = makelst (tail lst) (temp ++ [head lst]) fg


readi lst = makelst lst [] True

-- четене на входне данни


--прилагане на бфс
bfs matrix used current goal row column index
    | goal `elem` current = index
    | null current = -1
    | otherwise = bfs matrix  (used++ current)  (remsec ( remdups (qsort  (filter (\x -> inborders x row column )  (genneibors current) ) ) ) used ) goal row column (index+1)


genneibors lst = concat [ [[x,y+1 ] , [x,y-1] , [x-1 ,y] ,[x+1,y] ] | [x,y] <- lst ]


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




-- конкретно за големия пример по задаване 
-- за други входне данни и размерности на матрицата да се промени
func lst = bfs 0  lst  [[0,0]] [70,70] 71 71 0



-- оптимизиране с двоично търсене
func3 l r lst
  | l +1 == r = l
  | func (take m lst ) == (-1) =func3 l m lst
  |otherwise = func3 m r lst
  where m = div (l+r ) 2

func4 lst =(!!)  lst ( func3 0 (length lst ) lst )




main = do
  content <- readFile "day18.txt"
  let inp =map  readi (lines content)
  
  -- за 1 част търсен отговор
  print $ func  (take 1024 inp)
 -- за 2 част търсен отговор 
  print $ func4 inp

