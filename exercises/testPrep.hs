import Data.Char (toUpper, toLower)

-- Да се напише функция, която по подадени дробни числа x и y, както и числа r и R, 
-- проверява дали точката с координати (x, y) лежи се намира във венеца между кръговете с радиуси съответно r и R(приемаме, че r < R).
-- isWithinCircularBand :: (Double, Double) -> Double -> Double -> Bool
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

-- Да се напише функция, която по подаден списък от числа премахва всички повторни срещания на елементите си.
removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs) = if x `elem` xs then removeDuplicates xs else x : removeDuplicates xs 

-- Да се напише функция, която по подаден списък от числа
-- и число n връща наредена двойка, в която първият елемент е
--  списък от първите n числа в списъка, а вторият - списък от останалите елементи.
breakList :: [Int] -> Int -> ([Int], [Int])
breakList [] _ = ([], [])
breakList xs 0 = ([], xs)
breakList (x:xs) y = (x : first, second)
  where
    (first, second) = breakList xs (y - 1)

-- Да се напише функция, която по списък от наредени двойки от символ и число връща низ, получен по следния начин:
-- [(ch1, n1), ..., (chM, nM)] = n1 пъти символа ch1, n2 пъти символа ch2, ..., nM пъти символа chM
simpleDecode :: [(Char, Int)] -> [Char]
simpleDecode [] = ""
simpleDecode ((first, second):xs) = repeatChar first second ++ simpleDecode xs
  where
    repeatChar _ 0 = []
    repeatChar c n = c : repeatChar c (n - 1)

-- Да се напише функция, която слива два сортирани списъка.
mergeSortedLists :: [Int] -> [Int] -> [Int]
mergeSortedLists [] lst = lst
mergeSortedLists lst [] = lst
mergeSortedLists (x:xs) (y:ys) 
    | x > y = y : mergeSortedLists (x:xs) ys
    | x < y = x : mergeSortedLists xs (y:ys)
    | otherwise = x : mergeSortedLists xs ys

-- Да се реализира алгоритъма за бързо сортиране.
quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (x:xs) = lessOrEq ++ [x] ++ bigger
    where
        lessOrEq = quickSort [y | y <- xs, y <= x]
        bigger   = quickSort [y | y <- xs, y  > x]

-- Да се напише функция, която разделя подаден списък на списък от списъци всеки с дължина n.
--  В случай че дължината на изходния списък не се дели точно на n, да се игнорират последните елементи.
breakListOfLength :: [Int] -> Int -> [[Int]]
breakListOfLength [] _ = []
breakListOfLength xs n
  | length xs < n = [] 
  | otherwise = take n xs : breakListOfLength (skip xs n) n
  where
    skip [] _ = []
    skip (_:ys) 1 = ys
    skip (_:ys) k = skip ys (k-1)

-- Да се напише функция, която премахва излишните интервали от подаден низ,
--  т.е. низът да не започва или завършва с интервал/и 
-- и между всеки две последователни думи да има по точно един интервал. 
-- Ако няма думи в низа, резултатът да бъде празен списък.
removeSpaces :: [Char] -> [Char]
removeSpaces [] = []
removeSpaces (' ':str) = removeSpaces str
removeSpaces (s:' ':' ':str) = removeSpaces (s:' ':str)
removeSpaces (s:' ':s':str) = s:' ':removeSpaces (s':str)
removeSpaces (s:str) = s: removeSpaces str

-- Да се напише функция, която по подадено число n връща списък от първите n прости числа.
firstNPrimes :: Int -> [Int]
firstNPrimes 0 = []
firstNPrimes n = nextPrime 2 [] n
  where
    nextPrime _ primes 0 = primes
    nextPrime x primes n
      | isPrime x = nextPrime (x + 1) (primes ++ [x]) (n - 1)
      | otherwise = nextPrime (x + 1) primes n
    isPrime k = k > 1 && all (\d -> k `mod` d /= 0) [2 .. floor (sqrt (fromIntegral k))]


main :: IO ()
main = do
    print (firstNPrimes 1)  

    