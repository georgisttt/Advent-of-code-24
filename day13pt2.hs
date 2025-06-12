allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1
  | allexist lst1 ['0' .. '9'] = read lst1
  | otherwise = 0

buttonrow lst = map isallintegers [takeWhile (/= ',') $ tail (dropWhile (/= '+') lst), tail $ dropWhile (/= '+') $ tail $ dropWhile (/= '+') lst]

prizerow lst = map isallintegers [takeWhile (/= ',') $ tail (dropWhile (/= '=') lst), tail $ dropWhile (/= '=') $ tail $ dropWhile (/= '=') lst]

-- функцее за четене на вход

-- за какво ни е математика
-- теорема на крамер за намиране на решения на линейни уравнение
-- за 2 такива това е формулата
-- съответно d dx dy  - детерминанти

toeqv [fsta, fstb] [snda, sndb] [c1, c2] = [fromIntegral dx / fromIntegral d, fromIntegral dy / fromIntegral d]
  where
    d = fsta * sndb - (snda * fstb)
    dx = (c1 * sndb) - (c2 * snda)
    dy = fsta * c2 - (fstb * c1)

-- нищо не променено
func [] = []
func (x : y : z : xs) = if null x then func (y : z : xs) else toeqv (buttonrow x) (buttonrow y) (map (+ 10000000000000) (prizerow z)) : func xs

funci lst = sum $ map ((\[x, y] -> 3 * x + y) . (\[x, y] -> [floor x, floor y])) (filter (\[x, y] -> (floor x == ceiling x) && (floor y == ceiling y)) (func lst))

main = do
  content <- readFile "day13.txt"
  print (funci (lines content))
