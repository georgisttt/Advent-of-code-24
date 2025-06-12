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

iscorrect lst
  | length lst < 8 = 0
  | length lst > 12 = 0
  | takesubstring lst 0 3 /= "mul(" = 0
  | last lst /= ')' = 0
  | otherwise = numberswithseparator (takesubstring lst 4 (length lst - 2))

-- нищо по интересно освен вдигане или сваляне на флаг при прочитане на специалните думи

calc fg lst
  | null lst = 0
  | takesubstring lst 0 3 == "do()" = calc False (tail lst)
  | takesubstring lst 0 6 == "don't()" = calc True (tail lst)
  | fg = calc fg (tail lst)
  | takesubstring lst 0 3 == "mul(" = sum [iscorrect (takesubstring lst 0 x) | x <- [7 .. 13]] + calc fg (tail lst)
  | otherwise = calc fg (tail lst)

calci fg lst
  | null lst = fg
  | takesubstring lst 0 3 == "do()" = calci False (tail lst)
  | takesubstring lst 0 6 == "don't()" = calci True (tail lst)
  | otherwise = calci fg (tail lst)

calculate fg lst
  | null lst = 0
  | otherwise = calc fg (head lst) + calculate (calci fg (head lst)) (tail lst)

main = do
  content <- readFile "day3.txt"
  print (calculate False (lines content))
