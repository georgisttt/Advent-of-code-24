maketo lst temp
  | null lst = [temp]
  | head lst == ',' = [temp] ++ maketo (tail $ tail lst) []
  | otherwise = maketo (tail lst) (temp ++ [head lst])

-- четене на вход

ispref el lst
  | null el = True
  | length el > length lst = False
  | otherwise = el == take (length el) lst

-- построяване на префиксите на думата с определена дължина и помнене на броя им за оптимизация
buildel el cr dic index maxsize
  | index - 1 == length el = if not (any (\(a, b) -> a == el) cr) then 0 else head $ map snd $ filter (\(a, b) -> a == el) cr
  | otherwise = buildel el (filter (\(a, b) -> length a + maxsize >= index) (gropi (cr ++ [(x ++ y, a) | (x, a) <- cr, y <- dic, length (x ++ y) == index, ispref (x ++ y) el]))) dic (index + 1) maxsize

-- групиране на еднакви елементи
gropi [] = []
gropi [a] = [a]
gropi ((a, b) : (c, d) : xs) = if a == c then gropi ((a, b + d) : xs) else [(a, b)] ++ gropi ((c, d) : xs)

main = do
  content <- readFile "day19.txt"
  let out = lines content
  let k = concatMap (`maketo` []) (takeWhile (not . null) out)

  let z = tail $ dropWhile (not . null) out
  let maxisize = maximum (map length z)

  let m = maximum (map length k)

  -- по колко начина може да се построи празния
  let dd = map (\x -> buildel x [([], 1)] k 0 m) z

  -- оставено за визуализация dd
  print dd

  print $ sum dd