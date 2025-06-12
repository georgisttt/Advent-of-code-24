sizein el
  | el < 10 = 1
  | otherwise = 1 + sizein (div el 10)

split el = [div el p, mod el p]
  where
    p = 10 ^ div (sizein el) 2

addone el = [el + 1]

mult el = [el * 2024]

makeonstep el
  | el == 0 = addone el
  | even (sizein el) = split el
  | otherwise = mult el

-- функции от условието
-- единственото интересно е че връщаме спсък от елементи което е необходимо защото имаме случай да "произведем повеече от 1 елемент"

-- броя стъпки броя елементи
generate lst step
  | step == 25 = [length lst]
  | otherwise = generate (concatMap makeonstep lst) (step + 1)

func lst = sum $ generate lst 0

-- разнообразие с четене от конзолата защото входните данни са малко на брой

-- моите входни данни:
--  >>> func[0, 4 ,4979 ,24 ,4356119 ,914 , 85734 , 698829]
-- 188902
