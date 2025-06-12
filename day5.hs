-- функции за обработка на входа

-- функция която проверява списък с уникални елементи дале са поделементи на другата
allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

-- от низ от числа до числото
isallintegers lst1 fg
  | allexist lst1 ['0' .. '9'] = if fg then read lst1 else negate (read lst1)
  | otherwise = 0

-- обработка от низ с сепаратор до низ от числа
-- преизползвана от друга задача , за това има функционалност за четене и на отрицателни числа
makelst lst temp fg separator
  | null lst = [isallintegers temp fg]
  | head lst == '-' = makelst (tail lst) temp False separator
  | head lst == separator = isallintegers temp fg : makelst (tail lst) [] True separator
  | otherwise = makelst (tail lst) (temp ++ [head lst]) fg separator

-- конкретните функции за превръщането на рет от  речника или  списъците в данни с които ще работя
readdic lst = makelst lst [] True '|'

readlines lst = makelst lst [] True ','

-- функциите коит осе позват заради сепараторав входа от  който е оказан ""
dic lst = map readdic (takeWhile (not . null) lst)

arr lst = map readlines (tail $ dropWhile (not . null) lst)

-- провека за един ред дали може да се представи от елементите на речника
funcorder dic el
  | length el <= 1 = True
  -- проверка дали елементите от [глава , всички елемеенти на опашката ] са равни на броя на елементите в опашната
  -- ако да - продължи за опашката
  -- ако не върни лъжа
  | length (tail el) == length (filter (`elem` dic) [[head el, y] | y <- tail el]) = funcorder dic (tail el)
  | otherwise = False

-- функцита която се ползва в main часта
-- чете вход разделяго на речник елементи
-- проверява дали исканото свойство е изпълнено (filter  с фунцията funcorder )
-- взимане на елемента посредата на масива
-- създаване на наредени двойки от резултат и среден елемент (грозно ако е на  отделни функции , приемливо ако е в една  )
-- филтрираме по първи елемент който ни казва дали е изпълнено условито
-- взимане на стойностите и прилагане на sum за краен резултат
func lst = sum $ map snd $ filter fst $ map (\x -> (funcorder (dic lst) x, (!!) x (div (length x) 2))) (arr lst)

main = do
  -- четене от файл коректни данни спрямо усровието и извеждане на резултата

  content <- readFile "day5.txt"
  print (func $ lines content)
