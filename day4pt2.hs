-- преизползване на функцията от 1 част за намиране на диагонални елементи с леки промени за да помни по двойки елементите
getdiag a b c
  | length a <= 2 = []
  | otherwise = ([head a, head $ tail b, head $ tail $ tail c], [head $ tail $ tail a, head $ tail b, head c]) : getdiag (tail a) (tail b) (tail c)

-- итериране по матрицата
horidiag [] = []
horidiag [a] = []
horidiag [a, b] = []
horidiag (a : b : c : xs) = getdiag a b c ++ horidiag (b : c : xs)

-- променена функция за бройка
findcount lst = length $ filter (\(a, b) -> (a == "MAS" && b == "MAS") || (a == "SAM" && b == "MAS") || (a == "SAM" && b == "SAM") || (a == "MAS" && b == "SAM")) $ horidiag lst

main = do
  content <- readFile "day4.txt"

  print $ findcount (lines content)
