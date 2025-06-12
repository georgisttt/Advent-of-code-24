import Data.Bits (Bits (xor))

calccombo (a, b, c) el
  | el <= 3 = el
  | el == 4 = a
  | el == 5 = b
  | el == 6 = c
  | otherwise = -1

makemove (a, b, c) [] mem print = print
makemove (a, b, c) [x] mem print = print
makemove (a, b, c) (q : w : xs) mem print
  | q == 0 = makemove (div a (2 ^ calccombo (a, b, c) w), b, c) xs mem print
  | q == 2 = makemove (a, mod (calccombo (a, b, c) w) 8, c) xs mem print
  | q == 1 = makemove (a, xor b w, c) xs mem print
  | q == 3 = if a == 0 then makemove (a, b, c) xs mem print else makemove (a, b, c) (drop w mem) mem print
  | q == 4 = makemove (a, xor b c, c) xs mem print
  | q == 5 = makemove (a, b, c) xs mem (print ++ [mod (calccombo (a, b, c) w) 8])
  | q == 6 = makemove (a, div a (2 ^ calccombo (a, b, c) w), c) xs mem print
  | q == 7 = makemove (a, b, div a (2 ^ calccombo (a, b, c) w)) xs mem print


starti (a, b, c) prog = makemove (a, b, c) prog prog []


-- използвана само за намиране на финалния резултат 
qsort [] =[ ]
qsort  (x:xs)= qsort [y| y<-xs , y <=x] ++[x] ++  qsort [y| y<-xs , y >x]



--основната функция за решаване 
-- до колокото прочетох и разгледах моя вход
-- има само едно място на четене x брой итерации и делене на 8 на a като само числото по модул а има значение
-- така че функцията имитира използване на число в осмична бройна систеама и проверява дали резултата до моменат е верен и намира следващия елемент
-- за едно решение има повече от един начин да се представи затова се помнят решенията в списък 

findval lst temp prog
  | null lst = head $ qsort $ map (`div` 8) temp
  | otherwise = findval (init lst) [(x + y) * 8 | x <- [0 .. 7], y <- temp, head (starti (x + y, 0, 0) prog) == last lst] prog


-- функция за лесна употреба
func lst = findval lst [0 ..7 ] lst





-- >>> func [2 , 4,1 ,7 ,7,5 , 4,1,1,4,5,5,0,3,3,0]    
-- 109685330781408





