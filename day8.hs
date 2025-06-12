addpos lst column
  | null lst = []
  | head lst == '.' = addpos (tail lst) (column + 1)
  | otherwise = (head lst, column) : addpos (tail lst) (column + 1)

finddiff lst row
  | null lst = []
  | otherwise = map (\(a, b) -> (a, (row, b))) (addpos (head lst) 0) : finddiff (tail lst) (row + 1)

-- функции за номериране на елементи и запазване на елемента в тях 

addtogroups lst temp
    | null lst = [temp]
    | fst (head temp) /=  fst (head lst)  = temp : addtogroups (tail lst ) [head lst]
    |otherwise = addtogroups (tail lst ) (head lst : temp )



-- сортировач
qsort []=[]
qsort (x:xs) = qsort [y| y<- xs , y<=x] ++ [x] ++ qsort [y| y<- xs , y>x]


-- направена , за да не забравя ,и да подчертая от къде се тръгва 
func lst = addtogroups (tail lst) [head lst ]


-- генериране на исканете елементи като не се пишат при рефлексивните елементи на декартовия квадрат 
generarate lst = concat [[ (2*b - q ,2*c - w ) , (2*q - b ,2*w - c )   ]  |  (a,(b,c)) <- lst , (r,(q,w)) <- lst , (a,(b,c)) /= (r,(q,w))   ]

-- премахва последователни елементи ако те отговарят на дадено свойство 
remdub lst
    |length lst <2 = lst
    | head lst ==head (tail lst) = remdub (tail lst )
    | otherwise = head lst : remdub (tail lst)


-- дали са в границите на матрицата генерираните елементи 
funci lst  row col = [ (a,b ) |(a,b) <- lst , a>=0 , a <row , b >=0 , b <col ]


main = do
  content <- readFile "day8.txt"
  -- с малкки стъпки към успеха 
  -- провека на междинни етапи за тоов са много променливите 
  let c = qsort $ concat $ finddiff (lines content) 0
  let maxrow  = length (lines content)
  let maxcol = length (head (lines content) )
  let el = func c

  let genpos =remdub $  qsort $ concatMap generarate el
  let res = funci genpos maxrow maxcol
  print (length  res)