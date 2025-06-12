import Data.Bits (Bits (xor))

-- изчисляваме комбото на втория операнд
calccombo (a, b, c) el
  | el <= 3 = el
  | el == 4 = a
  | el == 5 = b
  | el == 6 = c
  | otherwise = -1

-- правене на стъпка спрямо какво е казано
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

-- да не се пише по два пъти едно и също нещо
starti (a, b, c) prog = makemove (a, b, c) prog prog []

-- моя вход
-- >>> starti (53437164,0,0) [2 , 4,1 ,7 ,7,5 , 4,1,1,4,5,5,0,3,3,0]
-- [2,1,0,4,6,2,4,2,0]
