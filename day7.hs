-- тривиална обработка на входните  данни
-- преизползвани с модификации от минали задачи
check lst target = iscompitable (tail lst) target (head lst)

allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1 = if allexist lst1 ['0' .. '9'] then read lst1 else 0

-- >>> isallintegers "1234"
-- 1234
converttarget lst = isallintegers $ takeWhile (/= ':') lst

makelst lst temp
  | null lst = [isallintegers temp]
  | head lst == ' ' = isallintegers temp : makelst (tail lst) []
  | otherwise = makelst (tail lst) (temp ++ [head lst])

makeelem lst = makelst (tail (dropWhile (/= ':') lst)) []

lister lst = makelst (tail $ tail $ dropWhile (/= ':') lst) ""

-- край на обработка на входа

-- рекурсивно проверява дали едното или другото
-- като оптимизация е следното : естествено а , естествено b  тогава за   a*b =c || a+b =c то c >=a
iscompitable lst target current
  -- горната оптимизация
  | target < current = False
  -- край елементи и получен точен резултат
  | null lst && target == current = True
  -- край но грешен лъжа
  | null lst = False
  -- или от сбора или плюса намираме рещение
  | otherwise = iscompitable (tail lst) target (current + head lst) || iscompitable (tail lst) target (current * (head lst))

-- първия начин за който съм се сетил да обработя резултата и да запазя елемента
func lst = (check (lister lst) (converttarget lst), converttarget lst)

main = do
  content <- readFile "day7.txt"
  -- намери сумата на построимите числа
  print (sum $ map snd $ filter fst $ map func (lines content))
