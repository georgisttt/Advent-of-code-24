-- взимане на подсписъци от къде до къде
-- много грозно написан признавам
takesubstring lst from to
  | length lst <= to = []
  | to == 0 = [head lst]
  | from == 0 = head lst : takesubstring (tail lst) from (to - 1)
  | otherwise = takesubstring (tail lst) (from - 1) (to - 1)

allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1 lst2
  | allexist lst1 ['0' .. '9'] && allexist lst2 ['0' .. '9'] = read lst1 * read lst2
  | otherwise = 0

numberswithseparator lst
  | notElem ',' lst = 0
  | otherwise = isallintegers (takeWhile (/= ',') lst) (tail (dropWhile (/= ',') lst))

-- функции за четене на вход  и взимане на подниз

-- проверки дали низ е коректен и ако да връщане на стойноста от него
iscorrect lst
  | length lst < 8 = 0
  | length lst > 12 = 0
  | takesubstring lst 0 3 /= "mul(" = 0
  | last lst /= ')' = 0
  | otherwise = numberswithseparator (takesubstring lst 4 (length lst - 2))

-- има размер на подсписъците които взимаме защото числата са от 1 до 3 цифри
calc lst
  | null lst = 0
  | takesubstring lst 0 3 == "mul(" = sum [iscorrect (takesubstring lst 0 x) | x <- [7 .. 13]] + calc (tail lst)
  | otherwise = calc $ tail lst

main = do
  content <- readFile "day3.txt"
  print (sum $ map calc (lines content))
