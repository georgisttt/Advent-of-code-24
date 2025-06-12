
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


-- намиране на всички групи с горния алгоритъм
iteratee matrix elements
  | null elements = []
  | otherwise =p : iteratee matrix (filter  (`notElem` p)  (tail elements))
    where
      p= bfs matrix [[]] [head elements]  (length matrix ) (length $ head matrix ) 0   (matrixgetelement matrix (head $ head elements)  (head $ tail  $  head  elements) )


genall matrix  = [[a,b ] | a <- [0.. length matrix-1] , b<- [0 .. length (head matrix )-1]   ]



-- имах янкави празни списъци за първи елемент и на бързо съм го оправил
func matrix =map tail $  iteratee matrix (genall matrix )




-- гледах случаи за ъгълчета и проверявах по малките входни примери докато всички не станаха правилни 
findborders [] alli =[]
findborders ( [a,b] :c) alli =
  [ ([a-1 , b ] `elem  ` alli ) && ([a , b-1 ] `elem ` alli ) && ([a-1 , b-1] `notElem ` alli )
  , ([a+1 , b ] `elem ` alli ) && ([a , b-1 ] `elem ` alli ) && ([a+1 , b-1] `notElem ` alli )
  , ([a , b+1 ] `elem ` alli ) && ([a-1 , b ] `elem ` alli ) && ([a-1 , b+1] `notElem ` alli )
  , ([a , b+1 ] `elem ` alli ) && ([a+1 , b ] `elem ` alli ) && ([a+1 , b+1] `notElem ` alli )
  , ([a-1 , b ] `notElem  ` alli ) && ([a , b-1 ] `notElem ` alli ) && ([a-1 , b-1] `elem ` alli )
  , ([a+1 , b ] `notElem ` alli ) && ([a , b-1 ] `notElem ` alli ) && ([a+1 , b-1] `elem ` alli )
  , ([a , b+1 ] `notElem ` alli ) && ([a-1 , b ] `notElem ` alli ) && ([a-1 , b+1] `elem ` alli )
  , ([a , b+1 ] `notElem ` alli ) && ([a+1 , b ] `notElem ` alli ) && ([a+1 , b+1] `elem ` alli )
  , ([a-1 , b ] `notElem  ` alli ) && ([a , b-1 ] `notElem ` alli ) && ([a-1 , b-1] `notElem ` alli )
  , ([a+1 , b ] `notElem ` alli ) && ([a , b-1 ] `notElem ` alli ) && ([a+1 , b-1] `notElem ` alli )
  , ([a , b+1 ] `notElem ` alli ) && ([a-1 , b ] `notElem ` alli ) && ([a-1 , b+1] `notElem ` alli )
  , ([a , b+1 ] `notElem ` alli ) && ([a+1 , b ] `notElem ` alli ) && ([a+1 , b+1] `notElem ` alli ) ]  ++ findborders c alli

func3 = map (length . (\x ->    filter id ( findborders x x) ))


main = do
  content <- readFile "day12.txt"
  let q =(lines content)

  let br = func q



  -- за отговора 
  print $ sum $  zipWith (* ) (map length  br) (func3  br )




  -- print $ func3 br  











-- aaa 