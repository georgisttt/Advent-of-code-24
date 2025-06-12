
-- четене на входните данни 
toarr lst = [takeWhile (/= '-') lst, tail $ dropWhile (/= '-') lst]

-- симетричност на релацията 
alli lst = concat [[[x, y], [y, x]] | [x, y] <- lst]

-- взимане на всички елементи 
alleelms  lst = map head lst

-- сортировач
qsort []=[]
qsort (x:xs) = qsort [y| y<-xs , y<=x] ++ [x] ++  qsort [y| y<-xs , y>x]

-- премахване на последователните еднакви  елементи 
remus lst
    | length lst <= 1 =lst
    | head lst == head (tail lst) = remus (tail lst )
    |otherwise = head lst : remus (tail lst )



-- генериране на всички 3 клики 
allcon  lst = [[x,y,z] |x<-p , head x =='t' , y<-  map (head .tail)   $ filter (\t -> head t ==x ) c , z<-  filter (\e -> [x,e]`elem ` c && [y,e ] `elem ` c  )    p      ]
    where
        p = remus $ qsort $ alleelms $ alli lst
        c=  alli lst

-- премахване на повтарящи се тройки 
remdup lst =  remus $ qsort $ map qsort  $ allcon lst



main = do
  content <- readFile "day23.txt"
  print (length $ remdup $  map toarr (lines content))
