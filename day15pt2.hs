matrixgetrow :: [[a]] -> Int -> [a]
matrixgetrow lst row = (!!) lst row

matrixgetelement :: [[a]] -> Int -> Int -> a
matrixgetelement lst row column = (!!) (matrixgetrow lst row) column

tran matrix
  | null (head matrix) = []
  | otherwise = [map head matrix] ++ (tran (map tail matrix))

canmoverwe lst
  | head lst /= '@' = canmoverwe (tail lst)
  | head lst == '@' && (head (filter (\x -> x == '.' || x == '#') lst)) == '.' = True
  | otherwise = False

{-

-- >>> canmover "#O..O@#.#"
-- False

--  ns diasno e kato nna liavo no reverse na lst v nachalototo

mover lst
  | canmover lst = (takeWhile (\x -> x /= '@') lst) ++ ['.'] ++ (takeWhile (\x -> x /= '.') (dropWhile (\x -> x /= '@') lst)) ++ (tail $ dropWhile (\x -> x /= '.') (dropWhile (\x -> x /= '@') lst))
  | otherwise = lst

-- >>> tran ["##########","#..O..O.O#","#......O.#","#.OO..O.O#","#..O@..O.#","#O#..O...#","#O..O..O.#","#.OO.O.OO#","#....O...#","##########"]

-- >>> tran  ["##########","#....OO..#","#..O.#.O.#","#O.OO..O.#","#...@.O..#","#....O.OO#","#O.O.....#","#.O.O.OO.#","#O.O...O.#","##########"]
-- ["##########","#..O..O.O#","#......O.#","#.OO..O.O#","#..O@..O.#","#O#..O...#","#O..O..O.#","#.OO.O.OO#","#....O...#","##########"]

makemove lst
  | null $ head lst = []
  | not $ null (filter (\x -> x == '@') (head lst)) = [mover (head lst)] ++ tail lst
  | otherwise = [head lst] ++ makemove (tail lst)

-- >>> makemove  ["##########","#..O..O.O#","#......O.#","#.OO..O.O#","#..O@..O.#","#O#..O...#","#O..O..O.#","#.OO.O.OO#","#....O...#","##########"]
--               ["##########","#..O..O.O#","#......O.#","#.OO..O.O#","#..O.@.O.#","#O#..O...#","#O..O..O.#","#.OO.O.OO#","#....O...#","##########"]

iterat matrix dir
  | null dir = matrix
  | head dir == '>' = iterat (makemove matrix) (tail dir)
  | head dir == '<' = iterat (map reverse (makemove (map reverse matrix))) (tail dir)
  | head dir == '^' = iterat (tran $ map reverse ((makemove (map reverse (tran matrix))))) (tail dir)
  | head dir == 'v' = iterat (tran (makemove (tran matrix))) (tail dir)

calrow row lst index
  | null lst = 0
  | head lst == 'O' = (100 * row) + index + calrow row (tail lst) (index + 1)
  | otherwise = calrow row (tail lst) (index + 1)

calcmatrix matrix index
  | null matrix = 0
  | otherwise = (calrow index (head matrix) 0) + (calcmatrix (tail matrix) (index + 1))
-}

makebigger lst
  | null lst = []
  | head lst == '#' = "##" ++ makebigger (tail lst)
  | head lst == 'O' = "{}" ++ makebigger (tail lst)
  | head lst == '.' = ".." ++ makebigger (tail lst)
  | head lst == '@' = "@." ++ makebigger (tail lst)

nmatrix matrix = map makebigger matrix

-- >>> tran ["####################","##....{}....{}..{}##","##............{}..##","##..{}{}....{}..{}##","##....{}@.....{}..##","##{}##....{}......##","##{}....{}....{}..##","##..{}{}..{}..{}{}##","##........{}......##","####################"]
-- ["##########","##########","#....{{..#","#....}}..#","#..{.#.{.#","#..}.#.}.#","#{.{{..{.#","#}.}}..}.#","#...@.{..#","#.....}..#","#....{.{{#","#....}.}}#","#{.{.....#","#}.}.....#","#.{.{.{{.#","#.}.}.}}.#","#{.{...{.#","#}.}...}.#","##########","##########"]

{- tran
[   "##########",
    "##########",
    "#....{{..#",
    "#....}}..#",
    "#..{.#.{.#",
    "#..}.#.}.#",
    "#{.{{..{.#",
    "#}.}}..}.#",
    "#...@.{..#",
    "#.....}..#",
    "#....{.{{#",
    "#....}.}}#",
    "#{.{.....#",
    "#}.}.....#",
    "#.{.{.{{.#",
    "#.}.}.}}.#",
    "#{.{...{.#",
    "#}.}...}.#",
    "##########",
    "##########"]
-}

addpos lst column
  | null lst = []
  | otherwise = [(head lst, column)] ++ addpos (tail lst) (column + 1)

addrow lst row
  | null lst = []
  | otherwise = [(map (\(a, b) -> (a, row, b)) (head lst))] ++ (addrow (tail lst) (row + 1))

{-[ "####################",
    "##....{}....{}..{}##",
    "##............{}..##",
    "##..{}{}....{}..{}##",
    "##....{}@.....{}..##",
    "##{}##....{}......##",
    "##{}....{}....{}..##",
    "##..{}{}..{}..{}{}##",
    "##........{}......##",
    "####################"]
-}

getel r c matrix = head $ filter (\(a, b, e) -> r == b && c == e) matrix

main = do
  content <- readFile "day15.txt"
  let inp = (lines content)
  let matrix = nmatrix $ takeWhile (\x -> not $ null x) inp
  let dir = concat $ tail $ dropWhile (\x -> not $ null x) inp
  let beg = concat (addrow ((map (\t -> addpos t 0) matrix)) 0)
  let st = getel 4 8 beg
  {-
  let make = iterat matrix dir
  let val = calcmatrix make 0-}
  -- let c = concat (addrow ((map (\t -> addpos t 0) (lines content))) 0)
  -- let q = filter (\(a, c, b) -> a == '@') c
  print $ beg
  print st