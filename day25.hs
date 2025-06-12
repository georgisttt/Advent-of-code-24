
-- чета входовете разпределям ги в двете групи и после проверявам всяко на колко други става и накрая събирам бройката
converti lst temp
  | null lst = [temp]
  | null $ head lst = temp : converti (tail lst) []
  | otherwise = converti (tail lst) (temp ++ [head lst])

inputdo lst = converti lst []

keys = filter (\x -> head x == ".....")

openers = filter (\x -> head x /= ".....")

makenumb = map (map (\y -> length (filter (== '#') y) - 1))

withone el = map (zipWith (+) el)

sizeone el lst = length $ filter (\x -> length (filter (< 6) x) == length x) (withone el lst)

calcone lst1 lst2
  | null lst1 = 0
  | otherwise = sizeone (head lst1) lst2 + calcone (tail lst1) lst2

transpose lst
  | null $ head lst = []
  | otherwise = map head lst : transpose (map tail lst)

main = do
  content <- readFile "day25.txt"
  let p = inputdo $ lines content
  let k = makenumb $ map transpose $ keys p
  let o = makenumb $ map transpose $ openers p
  let numbop = calcone k o

  print numbop
