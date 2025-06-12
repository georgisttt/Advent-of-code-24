allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1
  | allexist lst1 ['0' .. '9'] = read lst1
  | otherwise = 0

buttonrow lst = map isallintegers [takeWhile (/= ',') $ tail (dropWhile (/= '+') lst), tail $ dropWhile (/= '+') $ tail $ dropWhile (/= '+') lst]

prizerow lst = map isallintegers [takeWhile (/= ',') $ tail (dropWhile (/= '=') lst), tail $ dropWhile (/= '=') $ tail $ dropWhile (/= '=') lst]

-- функции за четене на входа

-- наивно търсене като се обходяят всички възможности
solver x y c = [[a, b] | a <- [1 .. (div c x)], b <- [div (c - (a * x)) y], a * x + b * y == c, a <= 100, b <= 100]

seceqv x y c lst = [[a, b] | [a, b] <- lst, a * x + b * y == c]

-- функциа за обхождане на елементите
toeqv [fsta, fstb] [snda, sndb] [c1, c2] = if null p then 0 else minimum p
  where
    p = map (\[a, b] -> 3 * a + b) (seceqv fstb sndb c2 (solver fsta snda c1))

-- функция за обхождане спямо входните данни
func [] = 0
func (x : y : z : xs) = if null x then func (y : z : xs) else toeqv (buttonrow x) (buttonrow y) (prizerow z) + func xs

main = do
  content <- readFile "day13.txt"
  print (func (lines content))
