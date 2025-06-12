goodkey el = (takeWhile (/= ':') el, tail $ dropWhile (/= ' ') el)

goodk el temp
  | null el = [temp]
  | head el == ' ' = temp : goodk (tail el) []
  | otherwise = goodk (tail el) (temp ++ [head el])

goodkey3 el = goodk el []

-- четтене на входните данни
-- (име р стойност)


-- логически операции за моя тип данни
andii (a, b) (c, d) = d == "1" &&  b == "1"
xori (a, b) (c, d) = b /= d
ori (a, b) (c, d) = b == "1" || d == "1"


gen el = if el then "1" else "0"


-- функция за едно обхождане на списъка и оценяване на списъка 
evaluate dic elems
  | null elems = dic
  | null q || null p = evaluate dic (tail elems)
  | v == "AND" = evaluate (dic ++ [(last $ head elems, gen $ andii (head p) (head q)   )]) (tail elems)
  | v == "OR" = evaluate (dic ++ [(last $ head elems, gen $ ori (head p) (head q)      )]) (tail elems)
  | v == "XOR" = evaluate (dic ++ [(last $ head elems, gen $ xori (head p) (head q)    )]) (tail elems)
  | otherwise = [("qqq", "www")]
  where
    p = filter (\x -> head (head elems)  == fst x)    dic
    q = filter (\x -> (!!) (head elems) 2 == fst x) dic
    v = (!!) (head elems) 1

-- map fst e dic
remuseless lst dic = filter (\x -> last x `notElem` dic) lst


-- докато всички изрази не бъдат оценени продължадаме с вече новооценените стойности и старите
dosteps key orav last
  | null orav = key
  | last == key = last
  | otherwise = dosteps p q key
  where
    p = evaluate key orav
    q = remuseless orav (map fst p)

-- сортшровач 
qsort [] =[ ]
qsort  (x:xs)= qsort [y| y<-xs , y <=x] ++[x] ++  qsort [y| y<-xs , y >x]


-- функции за намиране на крайния желан резултат
getgood lst =qsort $ filter  (\(a,b) -> head a =='z')  lst
toint  lst
    | null lst = 0
    |head lst =="0" = 2 * toint (tail lst)
    | otherwise = 1+(2 * toint (tail lst))


main = do
  content <- readFile "day24.txt"
  let inp = lines content
  let k = takeWhile (/= "") inp
  let d = tail (dropWhile (/= "") inp)
  let kk = map goodkey k
  let dd = map goodkey3 d
  let vals = dosteps kk dd []

  print $ toint  $  map snd  $ getgood vals





