goodkey el = (takeWhile (\x -> x /= ':') el, tail $ dropWhile (\x -> x /= ' ') el)

goodk el temp
  | null el = [temp]
  | head el == ' ' = [temp] ++ goodk (tail el) []
  | otherwise = goodk (tail el) (temp ++ [head el])

goodkey3 el = goodk el []

andii (a, b) (c, d) = (  d == "1" &&  b == "1" )

-- >>>andii (1, "0") (2, "0")
-- True


xori (a, b) (c, d) = b /= d

ori (a, b) (c, d) = b == "1" || d == "1"

gen el = if el then "1" else "0"

evaluate dic elems
  | null elems = dic
  | null q || null p = evaluate dic (tail elems)
  | v == "AND" = evaluate (dic ++ [((last $ head elems), gen $ andii (head p) (head q)   )]) (tail elems)
  | v == "OR" = evaluate (dic ++ [((last $ head elems), gen $ ori (head p) (head q)      )]) (tail elems)
  | v == "XOR" = evaluate (dic ++ [((last $ head elems), gen $ xori (head p) (head q)    )]) (tail elems)
  | otherwise = [("qqq", "www")]
  where
    p = filter (\x -> ( (head(head elems))  == fst x ))    dic
    q = filter (\x -> (((!!) (head elems) 2) == fst x )) dic
    v = (!!) (head elems) 1

-- map fst e dic
remuseless lst dic = filter (\x -> (last x) `notElem` dic) lst

-- >>> remuseless [[1,2,3] , [3,4,5]]  [1,2,3]
-- [[3,4,5]]

dosteps key orav last
  | null orav = key
  | last == key = last
  | otherwise = dosteps p q key
  where
    p = evaluate key orav
    q = remuseless orav (map fst p)
qsort [] =[ ]
qsort  (x:xs)= qsort[y| y<-xs , y <=x] ++[x] ++  qsort[y| y<-xs , y >x]




getgood lst =qsort $ (filter  (\(a,b) -> head a =='z')  lst) 


toint  lst 
    | null lst = 0
    |head lst =="0" = 2 * (toint (tail lst) )
    | otherwise = 1+(2 * (toint (tail lst) ))  

caclx  dd =  (toint$ map snd  (reverse $ qsort (filter (\(a,b) -> (head a ) =='x'  ) dd))) + (toint$  map snd $ reverse $ qsort (filter (\(a,b) -> (head a ) =='y'  ) dd))   




toarr el  
    | el ==1 = ["1"]
    | el==0 = ["0"]
    | mod el 2 ==1 =  toarr (div el 2 )  ++ ["1"] 
    | otherwise =   toarr (div el 2 ) ++  ["0"]

findz  dd = toarr (caclx dd )


finddif lst1 lst2 index 
    |null lst1 = []
    |head lst1 /= head lst2 = [index] ++ finddif (tail lst1 ) (tail lst2 ) (index +1)
    |otherwise = finddif (tail lst1 ) (tail lst2 ) (index +1)








main = do
  content <- readFile "day24.txt"
  let inp = (lines content)
  let k = takeWhile (\x -> x /= "") inp
  let d = tail (dropWhile (\x -> x /= "") inp)
  let kk = map goodkey k
  let dd = map goodkey3 d
  let vals = dosteps kk dd []
  let oriz = findz kk 
  let fakez =  map snd $  reverse $  getgood vals


  print $ caclx  kk
  print   $ map snd $  filter (\(a,b) -> head a =='z' ) $ qsort  vals
  print  $ reverse $ findz  vals 


