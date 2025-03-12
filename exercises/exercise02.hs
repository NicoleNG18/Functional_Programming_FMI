--23.1 a
makeListOfNElements :: Int->[Int]
makeListOfNElements n = take n [2,4..]

--23.1 b
getNOfArithmeticProg:: Int->Int->Int->[Int]
getNOfArithmeticProg a d n = take n (getArithmeticProg a d)

--23.1 v
getNOfFactorials :: Integer -> [Integer]
getNOfFactorials n = take (fromIntegral n) (getFactorials)

--23.1 g
getEvenNumbers::[Int]
getEvenNumbers = [x | x<-[0..], even x]

--23.1 d
getArithmeticProg:: Int->Int->[Int]
getArithmeticProg a d= [a,a+d..]

--23.1 e
getFactorials :: [Integer]
getFactorials = [factorial x | x <- [1..]]
  where
    factorial 0 = 1
    factorial k = k * factorial (k - 1)

--23.2 
numberDigitsReverse :: Integer -> [Integer]
numberDigitsReverse 0 = []
numberDigitsReverse n = (n `mod` 10) : numberDigitsReverse (n `div` 10)

--23.3
numberDigitsRevWithoutRepetition :: Integer -> [Integer]
numberDigitsRevWithoutRepetition n = removeDuplicates (digitsRev n)
  where
    digitsRev 0 = []
    digitsRev x = (x `mod` 10) : digitsRev (x `div` 10)

    removeDuplicates [] = []
    removeDuplicates (x:xs)
      | x `elem` xs = removeDuplicates xs
      | otherwise   = x : removeDuplicates xs

--23.4
divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n-1], n `mod` x == 0]

isPerfect :: Integer -> Bool
isPerfect n = sum (divisors n) == n

perfectNumbers :: Integer -> [Integer]
perfectNumbers n = [x | x <- [1..n], isPerfect x]

--23.5
countOccurrences :: String -> Char -> Int
countOccurrences [] _ = 0
countOccurrences (x:xs) y = if x == y then 1 + countOccurrences xs y else countOccurrences xs y

uniqueChars :: String -> String
uniqueChars [] = []
uniqueChars (x:xs)
  | x `elem` xs = uniqueChars xs 
  | otherwise   = x : uniqueChars xs 

histogram :: String -> [(Char, Int)]
histogram s = [(c, countOccurrences s c) | c <- uniqueChars s]


main :: IO ()
main = do
    -- print(makeListOfNElements 10)
    -- print(getArithmeticProg 2 3 5)
    -- print(getFactorials 6)
    -- print(getEvenNumbers)
    -- print(getArithmeticProg 2 3)
    -- print(getFactorials)
    -- print(numberDigitsReverse 12345)
    -- print(numberDigitsRevWithoutRepetition 123345)
    -- print(perfectNumbers 500)
    -- print(countOccurencies "abracadabra" 'b')(co)
    print(getNOfFactorials 5)

  