maketo lst temp
  | null lst = [temp]
  | head lst == ',' = temp : maketo (tail $ tail lst) []
  | otherwise = maketo (tail lst) (temp ++ [head lst])

-- чентене на входни данни

-- има вградена функция но реших и да си я напиша
ispref el lst
  | length el > length lst = False
  | otherwise = el == take (length el) lst

-- генериране на всички префикси на дадена дума по подаден речник
onestep dic el = [(el, x) | x <- dic, ispref x el]

-- премахване на префикс от дума
reduce = map (\(a, b) -> drop (length b) a)

-- за последна стъпка в търсенето // положително дъно на рекурсията
isrec = any (\(a, b) -> a == b)

-- дали дума се разпознава от конакатенация на елементи на речник
isreci el dic
  | null el = False
  | isrec p = True
  | otherwise = isreci (reduce p) dic
  where
    p = concatMap (onestep dic) el

-- упростен вариант за вход
func el dic = isreci [el] dic

-- оптимизация
-- не са ни нужни всички ключове в тази част
-- само тези които не са поостроими от други ключове
reducekeys lst temp
  | null lst = temp
  | func (head lst) (temp ++ tail lst) = reducekeys (tail lst) temp
  | otherwise = reducekeys (tail lst) (head lst : temp)

main = do
  content <- readFile "day19.txt"
  let out = lines content
  let k = concatMap (`maketo` []) (takeWhile (not . null) out)
  let kk = reducekeys k []
  let z = tail $ dropWhile (not . null) out
  let t = filter id (map (`func` kk) z)
  -- оставям t само за да се види че функцията наистина прави нещо докато се чака за резултат
  print t
  print (length t)
