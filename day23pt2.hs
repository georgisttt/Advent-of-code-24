import Data.List (intersect)

toarr lst = [takeWhile (\x -> x /= '-') lst, tail $ dropWhile (\x -> x /= '-') lst]

alli lst = concat [[[x, y], [y, x]] | [x, y] <- lst]

alleelms  lst = map head lst

qsort []=[]
qsort (x:xs) = qsort [y| y<-xs , y<=x] ++ [x] ++  qsort [y| y<-xs , y>x]





remus lst
    | length lst <= 1 =lst
    | head lst == (head $ tail lst) = remus (tail lst )
    |otherwise = [head lst ] ++ remus (tail lst )





-- създаване на кортеж от  елемент и масив от неговите съседи

generate lst  = [(x , map (head.tail ) (filter (\t -> x == head t )   (alli lst)  )) | x <- alleelms$ alli lst ]
addsize lst = [(a,b , length b ) |(a,b ) <- generate lst  ]
addsame  cur  lst = [ ]


inter lst = foldr1   (\x   y ->    x `intersect` y ) lst



--  оптимизация в ощия случай да не проверява върхове с малка степен
-- конкретно в тази задача не е същестево защото в примера ми всеки връх беше от 13 степен
useless lst index = [(a,b,c ) | (a,b,c )<- lst , c>index ]


beg lst  = [[a,y] | (a,b,c) <- lst , y <- b  ]


-- взимане на съседи на даден връх
getel el lst =head $ map (\(x,y,c) -> y)  $ filter  (\(a,b,c) -> a ==el   )  lst




oneste lst cur  =remus$ qsort  $ map  qsort $  [p : t | t<- cur , p <-inter (map (`getel` lst)  t  )]


funci lst cur index

  | length cur ==1 = [head cur]
  | null cur = []
  |otherwise =  funci  (filter  (\ (a,b,c) -> c > index )  lst )  (oneste lst cur ) (index +1)



-- проверка за наличие на ребра между всички избрани върхове
allconnected lst   el
    | null lst =True
    | otherwise = (head lst  `elem  `inter (map (`getel` el) (tail lst ))  )  && allconnected (tail lst ) el




--  алгоритъма на bron-kerbosch за намиране на клики ползвам 

algo cliq current lst  min
    | null current = [cliq]
    | otherwise  = concat [ algo (cliq ++ [x] )  (filter (/= x) ( current `intersect `getel x lst   ))    lst (max min x  ) | x<-current , x>min        ]




main = do
  content <- readFile "day23.txt"
  let p =remus $ qsort $ addsize $  map toarr (lines content)
  let q = map   (\(a,b,c) ->a) p
  let c = algo  []   q  p []
  let i = maximum $ map length c
  let d = filter  (\t -> length t ==i)   c
  let w =qsort $ head $  remus $ qsort $ map qsort d


  print w
