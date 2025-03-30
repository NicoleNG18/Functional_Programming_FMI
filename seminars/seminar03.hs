example:: [Int]
example = [ x | x<- [1..10], even x]

example1 :: [(Int,Int)]
example1 = [(a,b) | a<-[1..3],b<-[10..12]]

naturals = [0..]

firstNSquares :: Int->[Int]
firstNSquares n = square $ take n naturals
    where
        square[]=[]
        square (x:xs) = (x*x : square(xs))

isInList :: Int -> [Int] -> Bool
isInList _ [] = False
isInList a (x:xs) = if a == x then True else isInList a xs     

-- Да се напише функция, която по подаден списък от числа премахва всички повторни срещания на елементите си.
removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs) = if x `elem` xs then removeDuplicates xs else x : removeDuplicates xs 

removeDuplicatesVol2 :: [Int] -> [Int]
removeDuplicatesVol2 [] =[]
removeDuplicatesVol2 (x:xs) = x: removeDuplicatesVol2(remove x xs)
    where
         remove _ [] = []
         remove x (y:ys) = if x == y then removed else y: removed
            where 
                removed = remove x ys

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

firstNPrimesVol2 :: Int -> [Int]
firstNPrimesVol1 n = take n [ x | x<-[1..], isPrimeVol2 x]    
    where isPrimeVol2 n = [] == [del| del <- [2..floor(sqrt(fromIntegral(n)))], n `mod` del == 0]

main :: IO()
main = do
    print(removeDuplicatesVol2 [1,2,1,1])
