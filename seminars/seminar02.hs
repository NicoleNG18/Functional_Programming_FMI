import Data.Char (toUpper, toLower)

-- Да се напише функция, която по подадени дробни числа x и y, както и числа r и R, 
-- проверява дали точката с координати (x, y) лежи се намира във венеца между кръговете с радиуси съответно r и R(приемаме, че r < R).
isInWealth :: (Double, Double) -> Double -> Double -> Bool
isInWealth (x, y) radiusOne radiusTwo = radiusOne <= distance && distance <= radiusTwo
    where distance = sqrt squares
          squares = x^2 + y^2  

-- Да се напише функция, която проверява дали цифрите на подадено число образуват растяща редица.
isIncreasing :: Int -> Bool
isIncreasing x 
    | x < 10 = True 
    | otherwise = if x `mod` 10 >= (x `div` 10) `mod` 10 
                  then isIncreasing (x `div` 10)
                  else False

-- Да се напише функция, която приема списък от цели числа и връща тяхното произведение.
getMultiplication :: [Int] -> Int
getMultiplication [] = 1
getMultiplication [x] = x
getMultiplication (x:xs) = x * getMultiplication xs

-- Числа на Фибоначи
-- да се напише функция, която връща n-тото число на Фибоначи
-- да се напише такава итеративна функция.
nthFibonacci :: Int -> Int
nthFibonacci 1 = 1
nthFibonacci 2 = 1
nthFibonacci x = nthFibonacci (x - 1) + nthFibonacci (x - 2)

-- Да се напише функция, 
-- която по подаден списък от цели числа и цяло число връща индексът на 
-- първото срещане на подаденото число в списъка или -1, ако го няма.
getIndex :: [Int] -> Int -> Int
getIndex [] _ = -1 
getIndex (x:xs) y
    | x == y    = 0   
    | otherwise = 1 + getIndex xs y  

-- Да се напише функция, която обръща подаден символен низ.
reverseArr :: [Char] -> [Char]
reverseArr [] = []
reverseArr [x] = [x]
reverseArr (x:xs) = x : reverseArr xs

-- Да се напише функция, която обработва 
-- подаден символен низ по следния начин:
--  малките букви ги прави големи, големите малки, всички останали символи ги заменя с ' '.
transformLetter :: Char -> Char
transformLetter x 
    | x >= 'a' && x <= 'z' = toUpper x   
    | x >= 'A' && x <= 'Z' = toLower x 
    | otherwise = ' ' 

-- Function to change the entire list
changeList :: [Char] -> [Char]
changeList [] = []
changeList (x:xs) = transformLetter x : changeList xs

main :: IO ()
main = do
    print(changeList "nI2Kol")