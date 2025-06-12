allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1 = if allexist lst1 ['0' .. '9'] then read lst1 else 0

converttarget lst = isallintegers $ takeWhile (/= ':') lst

makelst lst temp
  | null lst = [isallintegers temp]
  | head lst == ' ' = isallintegers temp : makelst (tail lst) []
  | otherwise = makelst (tail lst) (temp ++ [head lst])

makeelem lst = makelst (tail (dropWhile (/= ':') lst)) []

lister lst = makelst (tail $ tail $ dropWhile (/= ':') lst) ""

func lst = (check (lister lst) (converttarget lst), converttarget lst)

-- до тук нищо ново

-- брой цифри на десетично число
digits lst
  | lst < 10 = 1
  | otherwise = 1 + digits (div lst 10)

-- промени само в края
iscompitable lst target current
  -- новата функция изпълнява трика и на 1 част
  | target < current = False
  | null lst && target == current = True
  | null lst = False
  -- добави толкова единици на настоящото колкото е дължината на второто и тогава ги събери (буквално дефиниция на залепване на естествени числа)
  | otherwise = iscompitable (tail lst) target (current + head lst) || iscompitable (tail lst) target (current * head lst) || iscompitable (tail lst) target (current * (10 ^ digits (head lst)) + head lst)

check lst target = iscompitable (tail lst) target (head lst)

main = do
  content <- readFile "day7.txt"
  -- нищо ново
  print (sum $ map snd $ filter fst $ map func (lines content))

-- P.S - 1 минута