

bfs  used  current goal row column borders  
    
    | null current = filter (\(a,b,c) ->a ==head goal  && b == (head $ tail goal)  ) used 
    | otherwise = [(length current , -100 , length used ) ]++ bfs  ( (map  (\ (a,b,c,d) -> (a,b,c)  ) current )++ used )    (filter    (\x -> isgood  (borders ++ used) x)   (concatMap genneibors current) )  goal row column  borders    






-- >>> isgood     [(1,2,101) , (1,2,(11) )]    (1,2 , 10 ,'^') 
-- False





isgood  used (a,b,c,d)  =  null p 
  where 
    p=filter  (\(r,s,w ) -> a ==r && b == s && c > w )   used      

-- concat [ [[x,y+1 ] , [x,y-1] , [x-1 ,y] ,[x+1,y] ] | [x,y] <- lst ]

genneibors (a,b,c,d)
  |d=='^' = [(a,b+1,c+1,'>'), (a,b-1,c+1,'<') , (a-1,b,c+1,'^')   ]
  |d=='>' = [(a,b+1,c+1,'>'), (a-1,b,c+1,'^') , (a+1,b,c+1,'v')  ]
  |d=='<' = [ (a,b-1,c+1,'<') , (a-1,b,c+1,'^') , (a+1,b,c+1,'v')  ]
  |d=='v' = [(a,b+1,c+1001,'>'), (a,b-1,c+1,'<')  , (a+1,b,c+1,'v')  ]


  


-- >>> bfs 0 [] [[0,0]] [4,5] 5 6 0 
-- 9







remsec temp lst
  | null temp = []
  | (head temp) `elem` lst = remsec (tail temp) lst
  | otherwise = head temp : remsec (tail temp) lst

remdups lst
  | null lst = []
  | length lst == 1 = lst
remdups ( (a,b,r) : (c,d,e):xs)
  | a == c && b == d   = remdups ((a,b,r) :xs)
  | otherwise = [(a,d,e)] ++ remdups xs


inborders el  row column = head el >=0 && head el < row && (!! ) el 1 >=0 && (!!)  el 1 <column 





-- >>> filter (\[x,y] -> inborders [x,y] 6 5  ) [[3,4] , [3,8] ,[7,3]]
-- [[3,4]]




qsort [] =[ ]
qsort  (x:xs)= qsort[y| y<-xs , y <=x] ++[x] ++  qsort[y| y<-xs , y >x]






-- >>> remdups [1,2,3,34,5,6,7,7,6,5,1]
-- [1,2,3,34,5,6,7,6,5,1]




matrixgetrow :: [[a]] -> Int -> [a]
matrixgetrow lst row = (!!) lst row



matrixgetelement :: [[a]] -> Int -> Int -> a
matrixgetelement lst row column = (!!) (matrixgetrow lst row) column





addpos lst column
  | null lst = []
  | otherwise = [(head lst, column)] ++ addpos (tail lst) (column + 1)

addrow lst row
  | null lst = []
  | otherwise = [(map (\(a, b) -> (a, row, b)) (head lst))] ++ (addrow (tail lst) (row + 1))



bad matrix = map  (\ (a,b,c) -> (b,c,(-1))) (filter (\(a,b,c) -> a=='#')  matrix) 


getgood lst = minimum (map (\(a,b,c) -> c)  (filter  (\ (a,b,c) -> b >0 )lst ))


--20:50

main = do
  content <- readFile "day16.txt"
  let matrix = lines content 
  let beg =  concat (addrow ((map (\t -> addpos t 0) matrix)) 0)
  
  let di = bad beg 
  let st =  map  (\ (a,b,c) -> (b,c , 0 , '>' )) (filter (\(a,b,c) -> a=='S')  beg) 
  let end = head $ map  (\ (a,b,c) -> [b,c]) (filter (\(a,b,c) -> a=='E')  beg) 
  let path = bfs  [] st end  (length matrix ) (length $ head matrix ) di  
  

  let val = getgood path 
  print path
  print $ val 
  
  print $ length $ (filter  (\(a,b,c) -> c == val)  path) 

  










