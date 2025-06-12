allexist lst1 lst2 = lst1 == filter (\x -> x `elem` lst2) lst1

isallintegers lst1 fg
  | allexist lst1 ['0' .. '9'] = if fg then read lst1 else 0 - (read lst1)
  | otherwise = 0

makelst lst temp fg sep
  | null lst = [isallintegers temp fg]
  | head lst == '-' = makelst (tail lst) temp False sep
  | head lst == sep = isallintegers temp fg : makelst (tail lst) [] True sep
  | otherwise = makelst (tail lst) (temp ++ [head lst]) fg sep

readidic lst = makelst lst [] True '|'

readlines lst = makelst lst [] True ','

dic lst = map readidic (takeWhile (not . null) lst)

arr lst = map readlines (tail $ dropWhile (not . null) lst)

funcorderi dic el
  | length el <= 1 = True
  | length el == 1 + length (filter (`elem` dic) [[head el, y] | y <- tail el]) = funcorderi dic (tail el)
  | otherwise = False

-- едно към едно от 1 част

--  мапване на елементите  на колко елементи е "баща"
funcorder dic bif aff
  | null aff = []
  | otherwise = (length (filter (`elem` dic) [[head aff, y] | y <- bif ++ tail aff]), head aff) : funcorder dic (bif ++ [head aff]) (tail aff)

func lst = map (((snd . head) . (\x -> filter (\(a, b) -> a == div (length x) 2) x)) . funcorder (dic lst) []) (arr lst)

-- трик който използавам
-- ако има парвилна единствена наредба то тя е винаги с един и съще елемент
-- което не ми е нужно да преннареждам списъка за да го намеря а да намеря елемента с дължина на списък делено на две деца

funcsort lst = map (fst . (\x -> (funcorderi (dic lst) x, (!!) x (div (length x) 2)))) (arr lst)

-- каквото пише в условието - на всички списъци с неправилна наредла намери сумата на средните елементи в правилната наредба

done lst = sum $ map snd $ filter (\(a, b) -> not a) $ zip (funcsort lst) (func lst)

main = do
  content <- readFile "day5.txt"
  print (done (lines content))
