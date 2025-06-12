convert0 lst
  | null lst = []
  | otherwise = [head lst] : convert0 (tail lst)

getdigtochar int val ch
  | null val = []
  | int == head val = [head ch]
  | otherwise = getdigtochar int (tail val) (tail ch)

converttoint el = head $ getdigtochar el ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] [0 .. 9]

convert1 lst index
  | null lst = []
  | length lst == 1 = [(head lst, index, 'n')]
  | otherwise = [(head lst, index, 'n'), (head $ tail lst, -1, '.')] ++ convert1 (tail $ tail lst) (index + 1)

isbad (a, b, c) = a == 0 || c == '.'

getone (a, b, c) = a

gettwo (a, b, c) = b

gett (a, b, c) = c

-- до тук нещо ново

-- итериращата функция както е дадена по условие за да види дали може да добави дадената група
addtolist el [] = []
addtolist (q, w, e) ((a, b, c) : xs)
  -- ако сме достигнали до него не го пипамеи спираме
  | q == a && w == b && e == c = (a, b, c) : xs
  | c == 'n' || q > a = (a, b, c) : addtolist (q, w, e) xs
  | otherwise = [(q, w, e)] ++ [(a - q, b, c)] ++ map (\qw -> if qw == (q, w, e) then (q, w, '.') else qw) xs

-- за ц+целия списък вече
convertit [] work = work
convertit ((a, b, c) : xs) work
  | c == '.' = convertit xs work
  | otherwise = convertit xs (addtolist (a, b, c) work)

func2 lst = convertit (reverse lst) lst

-- отделена функция за пресмятане на крайния резултат
calculate [] index = 0
calculate ((a, b, c) : xs) index
  | c == '.' = calculate xs (index + a)
  | a == 0 = calculate xs index
  | otherwise = b * index + calculate ((a - 1, b, c) : xs) (index + 1)

main = do
  content <- readFile "day9.txt"
  let r = map converttoint $ convert0 $ head (lines content)
  let c1 = convert1 r 0
  let c2 = func2 c1
  let c3 = calculate c2 0

  print c2
  print c3
