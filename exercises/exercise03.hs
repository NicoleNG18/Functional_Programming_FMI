import Data.Char (ord, chr)

--24.1
getName :: Int -> String
getName x 
  | x == 0    = "zero"
  | x == 1    = "one"
  | x == 2    = "two"
  | x == 3    = "three"
  | x == 4    = "four"
  | x == 5    = "five"
  | x == 6    = "six"
  | x == 7    = "seven"
  | x == 8    = "eight"
  | x == 9    = "nine"
  | otherwise = "Not in scope"

--24.2
myCommonPrefix :: [Int] -> [Int] -> [Int]
myCommonPrefix [] (x:xs) = []
myCommonPrefix (x:xs) [] = []
myCommonPrefix (x:xs) (y:ys) 
    | x/= y = []
    | otherwise = x : myCommonPrefix xs ys 

--24.3
evenOddL :: [Int] -> (Int, Int)
evenOddL xs = (evenCount, oddCount)
  where
    evenCount = length [x | x <- xs, even x]
    oddCount  = length xs - evenCount

--24.4
pivotLx :: Int -> [Int] -> ([Int], [Int])
pivotLx x xs = (l1, l2)
  where
    l1 = [y | y <- xs, y < x]
    l2 = [y | y <- xs, y >= x]

--24.5
changeListCharacters :: [Char] -> [Char]
changeListCharacters [] = []
changeListCharacters (x:xs)
    | x >= 'a' && x <= 'z' = (chr (ord x - 32)) : changeListCharacters xs
    | otherwise = x : changeListCharacters xs

-- 24.6
isPalindrome :: [Char] -> Bool
isPalindrome [] = True 
isPalindrome [x] = True 
isPalindrome xs = xs == reversedList xs 
  where
    reversedList [] = []
    reversedList (y:ys) = reversedList ys ++ [y]

--24.7
getPairs :: [Int] -> [(Int, Int)]
getPairs [] = []
getPairs [_] = []
getPairs (x:y:xs)
    | x < y     = (x, y) : getPairs (y:xs)
    | otherwise = getPairs (y:xs)  

--24.8      
groupsOf :: [Int] -> Int -> [[Int]]
groupsOf [] _ = []
groupsOf lst x 
    | length lst < x = []  
    | otherwise = take x lst : groupsOf (skip x lst) x 
  where
    skip n (x:xs) = if n > 0 then skip (n - 1) xs else (x:xs)

--24.9
flattenl :: [[a]] -> [a]
flattenl [] = []
flattenl (x:xs) = x ++ flattenl xs

--24.10
decodel :: [(Int, a)] -> [a]
decodel [] = []
decodel (x:xs) = appendElement x ++ decodel xs
  where 
    appendElement (first, second) 
      | first == 0 = []
      | otherwise = second : appendElement (first - 1, second)

--24.11
packl :: (Eq a) => [a] -> [[a]]
packl [] = []
packl (x:xs) = packGroup [x] xs  
  where
    packGroup acc [] = [acc]  
    packGroup acc (y:ys)
      | head acc == y = packGroup (y : acc) ys  
      | otherwise     = acc : packl (y:ys)  
 
--24.12
encodel :: (Eq a) => [a] -> [(Int, a)]
encodel [] = []
encodel (x:xs) = let (firstGroup, rest) = countOccurrences x xs in (length firstGroup, x) : encodel rest
  where
    countOccurrences y [] = ([y], []) 
    countOccurrences y (z:zs)
      | y == z    = let (group, remaining) = countOccurrences z zs in (y : group, remaining)
      | otherwise = ([y], z:zs) 

--24.13
--a)
remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove y (x:xs) 
    | x == y    = xs  
    | otherwise = x : remove y xs 

--b)
removeB :: (Eq a) => a -> [a] -> [a]
removeB _ [] = []
removeB y (x:xs)
    | x == y    = removeB y xs  
    | otherwise = x : removeB y xs 

--24.14
removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs) = if x `elem` xs then removeDuplicates xs else x : removeDuplicates xs 

--24.15
getBiggerThanSum :: [Int] -> [Int]
getBiggerThanSum [] = []
getBiggerThanSum (x:xs) = getBiggerThanSumHelper 0 (x:xs)  
  where
    getBiggerThanSumHelper _ [] = []
    getBiggerThanSumHelper sum (y:ys)
      | y > sum   = y : getBiggerThanSumHelper (sum + y) ys  
      | otherwise = getBiggerThanSumHelper (sum + y) ys 

--24.16
mergeevenodd :: [Int] -> [Int] -> [Int]
mergeevenodd _ [] = []
mergeevenodd [] _ = []
mergeevenodd (x:xs) (y:ys) = x : y : mergeevenodd xs ys

main :: IO ()
main = do
    print (mergeevenodd [1, 2, 3] [4, 5, 6])
