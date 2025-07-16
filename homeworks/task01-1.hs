-- Домашна работа по Функционално програмиране
-- Никол Георгиева - група 4 
main :: IO()
main = do
    print $ minWindowProduct [1, 2, 3, 4, 5, 0] 3
    print $ minWindowProduct [1, -2, 3, -4, 5, 0] 2
   
-- Част 1.
-- Задача 1. Да се напише функция, която по подаден списък от числов тип и нареден тип lst и естествено число k 
-- изчислява минималното произведение на k последователни елемента на списъка. Приемете, че 0 < k < length lst.
-- minWindowProduct [1, 2, 3, 4, 5, 0] 3 -- Очакван изход: 0
-- minWindowProduct [1, -2, 3, -4, 5, 0] 2 -- Очакван изход: -20

minWindowProduct :: [Int] -> Int -> Int
minWindowProduct [] _ = 0
minWindowProduct _ 0 = 0
minWindowProduct lst k = minimum (windowProducts lst k)
  where
    windowProducts xs n
      | length xs < n = []
      | otherwise = product (take n xs) : windowProducts (tail xs) n

-- Обяснение:
-- Ако списъкът е празен или k е 0, функцията връща 0 - еdge cases.
-- За всяка позиция в списъка взимаме k последователни елемента и изчисляваме техните произведения, 
-- събираме ги в windowProducts и връщаме най-малката стойност от този списък с произведения.
-- product е вградена функция, която приема списък от числа и връща произведението на всички елементи в списъка.
