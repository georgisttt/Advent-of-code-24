-- интересна задача
-- не всичко което е написано трябва да се прави
-- идея
-- стринговете с посоки нужни ли са
-- или дължините за натискане на елементите

-- представяме си човека и правим списък с колко натискания ни трябват (база)
-- за ниво n+1 ни трябва минималната сума за сбор от минималните преходи на елемент до елемент от ниво n
-- генерираме единствено всички низове от число до посоки

allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers lst1
  | allexist lst1 ['0' .. '9'] = read lst1
  | otherwise = 0

convertt lst
  | length lst <= 1 = [lst]
  | otherwise = [head lst] : convertt (tail lst)

-- четене на входните данни

-- от до друг елемент с минимални натискания на бутони
-- отворен за предложения за редуциране на кода за това
func0 "A" "0" = ["<A"]
func0 "A" "1" = ["^<<A", "<^<A"]
func0 "A" "2" = ["<^A", "^<A"]
func0 "A" "3" = ["^A"]
func0 "A" "4" = ["^^<<A", "^<^<A", "^<<^A", "<^^<A", "<^<^A"]
func0 "A" "5" = ["^^<A", "<^^A", "^<^A"]
func0 "A" "6" = ["^^A"]
func0 "A" "7" = ["^^^<<A", "^^<^<A", "^^^<<A", "^<^^<A", "<^^^<A", "^^<<^A", "^<^<^A", "<^^<^A", "^<<^^A", "<^<^^A"]
func0 "A" "8" = ["^^^<A", "^^<^A", "^<^^A", "<^^^A"]
func0 "A" "9" = ["^^^A"]
func0 "A" "A" = ["A"]
func0 "0" "A" = [">A"]
func0 "0" "0" = ["A"]
func0 "0" "1" = ["^<A"]
func0 "0" "2" = ["^A"]
func0 "0" "3" = ["^>A", ">^A"]
func0 "0" "4" = ["^^<A", "^<^A"]
func0 "0" "5" = ["^^A"]
func0 "0" "6" = ["^^>A", "^>^A", ">^^A"]
func0 "0" "7" = ["^^^<A", "^^<^A", "^<^^A"]
func0 "0" "8" = ["^^^A"]
func0 "0" "9" = ["^^^>A", "^^>^A", "^>^^A", ">^^^A"]
func0 "1" "0" = [">vA"]
func0 "1" "A" = [">>vA", ">v>A"]
func0 "1" "1" = ["A"]
func0 "1" "2" = [">A"]
func0 "1" "3" = [">>A"]
func0 "1" "4" = ["^A"]
func0 "1" "5" = ["^>A", ">^A"]
func0 "1" "6" = ["^>>A", ">>^A", ">^>A"]
func0 "1" "7" = ["^^A"]
func0 "1" "8" = ["^^>A", ">^^A", "^>^A"]
func0 "1" "9" = ["^^>>A", "^>^>A", "^>>^A", ">^^>A", ">^>^A", ">>^^A"]
func0 "2" "A" = ["v>A", ">vA"]
func0 "2" "0" = ["vA"]
func0 "2" "1" = ["<A"]
func0 "2" "2" = ["A"]
func0 "2" "3" = [">A"]
func0 "2" "4" = ["^<A", "<^A"]
func0 "2" "5" = ["^A"]
func0 "2" "6" = ["^>A", ">^A"]
func0 "2" "7" = ["^^<A", "^<^A", "<^^A"]
func0 "2" "8" = ["^^A"]
func0 "2" "9" = ["^^>A", ">^^A", "^>^A"]
func0 "3" "A" = ["vA"]
func0 "3" "0" = ["v<A", "<vA"]
func0 "3" "1" = ["<<A"]
func0 "3" "2" = ["<A"]
func0 "3" "3" = ["A"]
func0 "3" "4" = ["^<<A", "<^<A", "<<^A"]
func0 "3" "5" = ["^<A", "<^A"]
func0 "3" "6" = ["^A"]
func0 "3" "7" = ["^^<<A", "<<^^A", "^<<^A", "<^<^A", "^<^<A", "<^^<A"]
func0 "3" "8" = ["^^<A", "^<^A", "<^^A"]
func0 "3" "9" = ["^^A"]
func0 "4" "A" = [">>vvA", ">v>vA", ">vv>A", "v>>vA", "v>v>A"]
func0 "4" "0" = [">vvA", "v>vA"]
func0 "4" "1" = ["vA"]
func0 "4" "2" = ["v>A", ">vA"]
func0 "4" "3" = ["v>>A", ">v>A", ">>vA"]
func0 "4" "4" = ["A"]
func0 "4" "5" = [">A"]
func0 "4" "6" = [">>A"]
func0 "4" "7" = ["^A"]
func0 "4" "8" = ["^>A", ">^A"]
func0 "4" "9" = ["^>>A", ">^>A", ">>^A"]
func0 "5" "0" = ["vvA"]
func0 "5" "A" = ["vv>A", "v>vA", ">vvA"]
func0 "5" "1" = ["v<A", "<vA"]
func0 "5" "2" = ["vA"]
func0 "5" "3" = ["v>A", ">vA"]
func0 "5" "4" = ["<A"]
func0 "5" "5" = ["A"]
func0 "5" "6" = [">A"]
func0 "5" "7" = ["^<A", "<^A"]
func0 "5" "8" = ["^A"]
func0 "5" "9" = ["^>A", ">^A"]
func0 "6" "0" = ["vv<A", "<vvA", "v<vA"]
func0 "6" "A" = ["vvA"]
func0 "6" "1" = ["v<<A", "<<vA", "<v<A"]
func0 "6" "2" = ["v<A", "<vA"]
func0 "6" "3" = ["vA"]
func0 "6" "4" = ["<<A"]
func0 "6" "5" = ["<A"]
func0 "6" "6" = ["A"]
func0 "6" "7" = ["^<<A", "<^<A", "<<^A"]
func0 "6" "8" = ["^<A", "<^A"]
func0 "6" "9" = ["^A"]
func0 "7" "0" = [">vvvA", "v>vvA", "vv>vA"]
func0 "7" "A" = [">>vvvA", ">v>vvA", ">vv>vA", ">vvv>A", "v>>vvA", "v>v>vA", "v>vv>A", "vv>>vA", "vv>v>A"]
func0 "7" "1" = ["vvA"]
func0 "7" "2" = [">vvA", "v>vA", "vv>A"]
func0 "7" "3" = [">>vvA", ">v>vA", ">vv>A", "v>>vA", "v>v>A", "vv>>A"]
func0 "7" "4" = ["vA"]
func0 "7" "5" = ["v>A", ">vA"]
func0 "7" "6" = ["v>>A", ">v>A", ">>vA"]
func0 "7" "7" = ["A"]
func0 "7" "8" = [">A"]
func0 "7" "9" = [">>A"]
func0 "8" "A" = [">vvvA", "v>vvA", "vv>vA", "vvv>A"]
func0 "8" "0" = ["vvvA"]
func0 "8" "1" = ["<vvA", "v<vA", "vv<A"]
func0 "8" "2" = ["vvA"]
func0 "8" "3" = [">vvA", "v>vA", "vv>A"]
func0 "8" "4" = ["<vA", "v<A"]
func0 "8" "5" = ["vA"]
func0 "8" "6" = [">vA", "v>A"]
func0 "8" "7" = ["<A"]
func0 "8" "8" = ["A"]
func0 "8" "9" = [">A"]
func0 "9" "0" = ["<vvvA", "v<vvA", "vv<vA", "vvv<A"]
func0 "9" "A" = ["vvvA"]
func0 "9" "1" = ["<v<vA", "<vv<A", "v<<vA", "v<v<A", "<<vvA", "vv<<A"]
func0 "9" "2" = ["<vvA", "v<vA", "vv<A"]
func0 "9" "3" = ["vvA"]
func0 "9" "4" = ["<<vA", "<v<A", "v<<A"]
func0 "9" "5" = ["<vA", "v<A"]
func0 "9" "6" = ["vA"]
func0 "9" "7" = ["<<A"]
func0 "9" "8" = ["<A"]
func0 "9" "9" = ["A"]

-- за позициите
func1 'A' '^' = ["<A"]
func1 'A' '>' = ["vA"]
func1 'A' 'v' = ["<vA", "v<A"]
func1 'A' '<' = ["v<<A", "<v<A"]
func1 'A' 'A' = ["A"]
func1 '^' 'A' = [">A"]
func1 '^' '^' = ["A"]
func1 '^' '<' = ["v<A"]
func1 '^' 'v' = ["vA"]
func1 '^' '>' = [">vA", "v>A"]
func1 '<' '^' = [">^A"]
func1 '<' 'A' = [">>^A", ">^>A"]
func1 '<' '<' = ["A"]
func1 '<' 'v' = [">A"]
func1 '<' '>' = [">>A"]
func1 'v' 'A' = [">^A", "^>A"]
func1 'v' '^' = ["^A"]
func1 'v' '<' = ["<A"]
func1 'v' 'v' = ["A"]
func1 'v' '>' = [">A"]
func1 '>' '^' = ["<^A", "^<A"]
func1 '>' 'A' = ["^A"]
func1 '>' '<' = ["<<A"]
func1 '>' 'v' = ["<A"]
func1 '>' '>' = ["A"]

adda lst = "A" ++ lst

-- от число до първи ботончета
calcfun1 lst temp
  | length lst <= 1 = temp
  | otherwise = calcfun1 (tail lst) [x ++ y | x <- temp, y <- (func0 (head lst) (head $ tail lst))]

-- от човешката гледна точка преместването от единия до другия и натискането му е със стойност 1
f0 = [(x, y, 1) | x <- ['A', 'v', '^', '<', '>'], y <- ['A', 'v', '^', '<', '>']]

-- намиране на стойноста на даден  израз по подаден речник с дължините
findval dic str
  | length str <= 1 = 0
  | otherwise = head (map (\(a, b, c) -> c) (filter (\(a, b, c) -> a == head str && b == head (tail str)) dic)) + findval dic (tail str)

-- генериране на ниво n+1 от ниво n
-- за втора част
-- за първа е същото единствената разлика че дъното е до ниво 2
-- за всеки елемнент на декартовото произведение от позиции намира минималния брой стъпки по подаден речник
iterat dic index
  | index == 25 = dic
  | otherwise = iterat [(x, y, minimum (map (findval dic . ("A" ++)) (func1 x y))) | x <- ['A', 'v', '^', '<', '>'], y <- ['A', 'v', '^', '<', '>']] (index + 1)

-- за 1 част
iterat1 dic index
  | index == 2 = dic
  | otherwise = iterat1 [(x, y, minimum (map (\x -> findval dic x) ((map (\x -> "A" ++ x) (func1 x y))))) | x <- ['A', 'v', '^', '<', '>'], y <- ['A', 'v', '^', '<', '>']] (index + 1)

-- лесни за използванее функции
funcfinddic1 = iterat1 f0 0

funcfinddic = iterat f0 0

-- пресмятане на финалния резултат от генерирания от низа число -> посоки
calculate el
  | length el <= 1 = 0
  | otherwise = head (map (\(a, b, c) -> c) (filter (\(a, b, c) -> (a == head el) && b == (head $ tail el)) funcfinddic)) + (calculate (tail el))

calculate1 el
  | length el <= 1 = 0
  | otherwise = head (map (\(a, b, c) -> c) (filter (\(a, b, c) -> (a == head el) && b == (head $ tail el)) funcfinddic1)) + (calculate1 (tail el))

main = do
  content <- readFile "day21.txt"

  let c = map (isallintegers . takeWhile (/= 'A')) (lines content)
  let begin = map (convertt . adda) (lines content)
  let fs = map (\x -> map ("A" ++) (calcfun1 x [[]])) begin

  let q = map (minimum . map calculate) fs
  let q1 = map (minimum . map calculate1) fs

  --  използвам го за намиране на последния търсен резултат
  let final = sum (zipWith (*) c q)
  let final1 = sum (zipWith (*) c q1)

  -- за 1 част отговор
  print final1
  -- за 2 част отговор
  print final
