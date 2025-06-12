import Data.Bits (Bits (xor))

func1 :: Int -> Int
func1 el = mod (xor (el * 64) el) 16777216

func2 :: Int -> Int
func2 el = xor (div el 32) el

func3 :: Int -> Int
func3 el = mod (xor (el * 2048) el) 16777216

iterat :: Int -> Int
iterat el = func3 $ func2 $ func1 el

fun :: Int -> Int -> [Int]
fun index el
  | index == 2000 = [el]
  | otherwise = el : fun (index + 1) (iterat el)

endi :: Int -> [Int]
endi = fun 0

allexist :: (Eq a) => [a] -> [a] -> Bool
allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers :: String -> Int
isallintegers lst1
  | allexist lst1 ['0' .. '9'] = read lst1
  | otherwise = 0

-- същото както първата задача

-- идея
-- ако се прави обхожддане на всяка наредена четворка е много бавно
-- обхождат се само да речем топ 300 на най често срещаните
-- може да се забележи че числения резултат намалява драстично с намаляване на срещанията
-- максимум се добавя числото 9 така чеч честотата е по важна в някаква степен от самото число

-- хистограма на броя срещания
histogr [] = []
histogr lst = (length lst - length p, head lst) : histogr p
  where
    p = filter (\x -> x /= head lst) lst

-- събиране на еднакви елементи
conci [] = []
conci [a] = [a]
conci ((a, s) : (q, w) : xs) = if a == q then conci ((a, s + w) : xs) else (a, s) : conci ((q, w) : xs)

-- сортиране
qsort [] = []
qsort (x : xs) = qsort [y | y <- xs, y >= x] ++ [x] ++ qsort [y | y <- xs, y < x]

-- комбиниране на поредица от числа и резултат който би трябвало да върнат
findit ([], []) = []
findit ([a], [b]) = []
findit ([a, z], [b, q]) = []
findit ([a, z, x], [b, k, j]) = []
findit (q : w : e : r : xs, a : s : d : f : ds) = ([q, w, e, r], f) : findit (w : e : r : xs, s : d : f : ds)

-- създаване на разликите в маркета
finddif [] cr = []
finddif (x : xs) cr = (x - cr) : finddif xs x

-- обхождане на всички входни редове функция
makefun lst = finddif (tail lst) (head lst)

-- за всяка група намери стойноста или ако я няма върни 0
-- каквото е написано в условието ...
find lst el = if null p then 0 else snd $ head p
  where
    p = filter (\(a, b) -> a == el) lst

main :: IO ()
main = do
  content <- readFile "day22.txt"
  let r = map isallintegers (lines content)

  let q = map (map (`mod` 10) . endi) r
  let rr = map makefun q

  let wwe = zip rr (map tail q)

  let el = map findit wwe

  -- 300 за да се обходи повече наредени четворки
  -- на собствен риск намаляване на бройката за да се намери по бързо но с потенцялна загуба на коректно решение
  let q = map (\(a, b) -> sum (map (`find` b) el)) $ take 300 $ qsort $ map (\(a, b) -> (b, a)) $ conci $ qsort $ concat el

  print q
  print $ maximum q
  print $ maximum q
