-- намиране на думи с дължина 4
getdiag a b c d
  | null a || null d = []
  | otherwise = [[head a, head b, head c, head d]] ++ getdiag (tail a) (tail b) (tail c) (tail d)

-- намиране на диагонални и вертикални
horidiag [] = []
horidiag [a] = []
horidiag [a, b] = []
horidiag [a, b, c] = []
horidiag (a : b : c : d : xs) = getdiag a b c d ++ getdiag a (drop 1 b) (drop 2 c) (drop 3 d) ++ getdiag (drop 3 a) (drop 2 b) (drop 1 c) d ++ horidiag (b : c : d : xs)

-- намиране на хоризонтални елементи
hor [] = []
hor [a] = []
hor [a, b] = []
hor [a, b, c] = []
hor (a : b : c : d : xs) = [a, b, c, d] : hor (b : c : d : xs)

allhor lst
  | null lst = []
  | otherwise = hor (head lst) ++ allhor (tail lst)

-- функция за бройката
findcount lst = length $ filter (\x -> x == "XMAS" || x == "SAMX") p
  where
    p = allhor lst ++ horidiag lst

main = do
  content <- readFile "day4.txt"
  -- putStrLn $ show (lines content)
  print $ findcount (lines content)
