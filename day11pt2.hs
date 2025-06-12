
-- първия път в който се зблъсках с проблема за пазене на бройка в този проект 

sizein el
  | el < 10 = 1
  | otherwise = 1 + sizein (div el 10)

split el count= [(div el p , count),( mod el p , count)]
  where
    p = 10 ^ div (sizein el) 2

addone el count = [ ( el + 1 , count)]

mult el  count = [( el * 2024 ,  count) ]

makeonstep el
  | el == 0 = addone el
  | even (sizein el) = split el
  | otherwise = mult el


-- до тук нищо ново , само добавен елемент за пазене на бройката  


-- сортировач
qsort [] =[]
qsort (x:xs) = qsort [y|y<- xs , y<=x] ++[x] ++ qsort [y|y<- xs , y>x]



-- премахване на доблиращите елементи като събира срещанията 
merg lst index count
    | null lst = [(index , count)]
    | fst (head lst) == index = merg (tail lst ) index (count + snd (head lst))
    | otherwise = (index , count ) : uncurry (merg (tail lst )) (head lst)

-- за бързо въвеждане е създадена и да не сучи потребителя за типове 
funnu  lst = merg (tail lst)  (fst $ head lst )  (snd $ head lst )




count  lst step
  | step == 75= sum $ map snd  lst
  | otherwise =    count (funnu $ qsort (concatMap (\(x,y ) -> makeonstep x y  ) lst)) (step + 1)



-- основна функция
cunc lst = count (map (\x -> (x , 1 ))  lst  ) 0


-- мои входни данни
-- >>> cunc  [0 ,4 ,4979 ,24 ,4356119, 914, 85734 ,698829]
-- 223894720281135

