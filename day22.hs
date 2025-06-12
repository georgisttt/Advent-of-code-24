import Data.Bits (Bits (xor))

allexist :: (Eq a) => [a] -> [a] -> Bool
allexist lst1 lst2 = lst1 == filter (`elem` lst2) lst1

isallintegers :: String -> Int
isallintegers lst1
  | allexist lst1 ['0' .. '9'] = read lst1
  | otherwise = 0

-- четене на входни данни

-- следвам условието и го пиша както е казано

func1 :: Int -> Int
func1 el = mod (xor (el * 64) el) 16777216

func2 :: Int -> Int
func2 el = xor (div el 32) el

func3 :: Int -> Int
func3 el = mod (xor (el * 2048) el) 16777216

iterat :: Int -> Int
iterat el = func3 $ func2 $ func1 el

fun :: Int -> Int -> Int
fun index el
  | index == 2000 = el
  | otherwise = fun (index + 1) (iterat el)

endi :: Int -> Int
endi = fun 0

main :: IO ()
main = do
  content <- readFile "day22.txt"
  let r = map (endi . isallintegers) (lines content)
  print $ sum r
