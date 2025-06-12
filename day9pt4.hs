convert0 lst
  | null lst = []
  | otherwise = [head lst] : convert0 (tail lst)

getdigtochar int val ch
  | null val = []
  | int == head val = [head ch]
  | otherwise = getdigtochar int (tail val) (tail ch)

converttoint el = head $ getdigtochar el ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] [0 .. 9]

-- четене на елементите

-- преобразуване до подходящ за бпръзо намиране тип
convert1 lst index
  | null lst = []
  | length lst == 1 = [(head lst, index, 'n')]
  | otherwise = [(head lst, index, 'n'), (head $ tail lst, -1, '.')] ++ convert1 (tail $ tail lst) (index + 1)

-- помощни грозни функции  или помощно грозни функции
isbad (a, b, c) = a == 0 || c == '.'

getone (a, b, c) = a

gettwo (a, b, c) = b

gett (a, b, c) = c

-- итерираща функция точно копие на условито по условието
makemoves index [] = 0
makemoves index ((0, b, c) : xs) = makemoves index xs
makemoves index ((a, b, 'n') : xs) = b * index + makemoves (index + 1) ((a - 1, b, 'n') : xs)
makemoves index x
  | isbad (last x) = makemoves index (init x)
  | otherwise = index * gettwo (last x) + makemoves (index + 1) ([(getone (head x) - 1, -1, '.')] ++ tail (init x) ++ [(getone (last x) - 1, gettwo (last x), 'n')])

func = makemoves 0

main = do
  content <- readFile "day9.txt"

  let r = map converttoint $ convert0 $ head (lines content)
  let c1 = convert1 r 0
  let r = func c1

  --  print c1
  print r
