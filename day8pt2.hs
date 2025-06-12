
-- същата идея като първа част ще пиша само там където има промени 
addpos lst column
  | null lst = []
  | head lst == '.' = addpos (tail lst) (column + 1)
  | otherwise = (head lst, column) : addpos (tail lst) (column + 1)

finddiff lst row
  | null lst = []
  | otherwise = map (\(a, b) -> (a, (row, b))) (addpos (head lst) 0) : finddiff (tail lst) (row + 1)

qsort []=[]
qsort (x:xs) = qsort [y| y<- xs , y<=x] ++ [x] ++ qsort [y| y<- xs , y>x]


addtogroups lst temp
    | null lst = [temp]
    | fst (head temp) /=  fst (head lst)  = temp : addtogroups (tail lst ) [head lst]
    |otherwise = addtogroups (tail lst ) (head lst : temp )
func lst = addtogroups (tail lst) [head lst ]


-- третия елемент е вектор посока който той е в основата къде ще са останалите точки 
-- права може да се представи с начална точка и вектор 
generarate lst = concat [[ ((b,c) , (q,w) , (b-q , c-w))]  |  (a,(b,c)) <- lst , (r,(q,w)) <- lst , (a,(b,c)) /= (r,(q,w))   ]

remdub lst
    |length lst <2 = lst
    | head lst ==head (tail lst) = remdub (tail lst )
    | otherwise = head lst : remdub (tail lst)

funci lst  row col = [ (a,b ) |(a,b) <- lst , a>=0 , a <row , b >=0 , b <col ]

-- генератор на точките
-- ако не се получава коректен отговор ако матрицата е с размери по големи от 200x200 човека който иска да го ползва да промени -100 до 100 за сметка на оптималнста за време 
generate ((a,b) , (c,d) , (q,w)) col row  = filter   (\x  ->   fst x>=0 &&  fst x < row &&  snd x  >=0 &&  snd x < col )   [(a +( i *q)   , b+( w*i)  ) |i <- [-100 .. 100   ]      ]

main = do
  content <- readFile "day8.txt"
  let c = qsort $ concat $ finddiff (lines content) 0
  let maxrow  = length (lines content)
  let maxcol = length (head (lines content) )
  let el = func c
  let fu =  qsort $ concatMap generarate el
  let eli = remdub$  qsort $ concatMap (\x -> generate x maxrow maxcol) fu

  print (length  eli)