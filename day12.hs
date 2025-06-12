
-- бфс за да обходим групите от елементи 
-- заедно с помощни функции
-- с варияция с дъно празното множество от настоящи елементи 
bfs matrix used current  row column index  letter
  | null current = used
  | otherwise = bfs matrix (used ++ current) (remsec (remdups (qsort (filter (\[a,b] -> inborders [a,b] row column &&  matrixgetelement  matrix a b ==letter ) (genneibors current)))) used)  row column (index + 1) letter

genneibors lst = concat [[[x, y + 1], [x, y - 1], [x - 1, y], [x + 1, y]] | [x, y] <- lst]

matrixgetelement lst row = (!!) ((!!) lst row)

qsort []=[]
qsort (x:xs) = qsort [y| y<- xs , y<=x] ++ [x] ++ qsort [y| y<- xs , y>x]

remsec temp lst
  | null temp = []
  | head temp `elem` lst = remsec (tail temp) lst
  | otherwise = head temp : remsec (tail temp) lst

remdups lst
  | null lst = []
  | length lst == 1 = lst
  | head lst == head (tail lst) = remdups (tail lst)
  | otherwise = head lst : remdups (tail lst)

inborders el row column = head el >= 0 && head el < row && (!!) el 1 >= 0 && (!!) el 1 < column




iteratee matrix elements
  | null elements = []
  | otherwise =p : iteratee matrix (filter  (`notElem` p)  (tail elements))
    where
      p= bfs matrix [[]] [head elements]  (length matrix ) (length $ head matrix ) 0   (matrixgetelement matrix (head $ head elements)  (head $ tail  $  head  elements) )



genall matrix  = [[a,b ] | a <- [0.. length matrix-1] , b<- [0 .. length (head matrix )-1]   ]


-- наивно обхождане на броя стени 
countborders [a,b] lst  = length (filter id [[a,b+1] `notElem ` lst  ,  [a-1,b] `notElem ` lst  ,[a,b-1] `notElem ` lst  ,[a+1,b] `notElem ` lst    ]   )




func matrix =map tail $  iteratee matrix (genall matrix )

-- функция за всяка група да намери стените 
numborders = map  (\c ->sum $  map (`countborders` c) c   )


main = do
  content <- readFile "day12.txt"
  let q =lines content

  let br = func q


  print $ sum $  zipWith (* ) (numborders br) (map length br )

