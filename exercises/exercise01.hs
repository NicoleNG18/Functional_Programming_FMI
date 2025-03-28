-- 22.1
triangleArea :: Double->Double->Double
triangleArea c hc = 0.5 * c * hc 

semiPerimeter:: Double->Double->Double->Double
semiPerimeter a b c = (a + b + c) * 0.5

triangleAreaSecond:: Double->Double->Double->Double
triangleAreaSecond a b c = 
    let sp = semiPerimeter a b c 
    in sqrt (sp * (sp - a) * (sp - b) * (sp - c))

-- 22.2
placeByCoordinateSystem :: Int -> Int -> String
placeByCoordinateSystem x y
    | x == 0 && y == 0     = "the beginning"
    | x == 0               = "on abscis"
    | y == 0               = "on ordinate"
    | x > 0 && y > 0       = "first"
    | x < 0 && y > 0       = "second"
    | x < 0 && y < 0       = "third"
    | x > 0 && y < 0       = "fourth"
    | otherwise            = "Unknown position"

--22.3 - a
isDivisibleByFourOrSeven :: Int -> Bool
isDivisibleByFourOrSeven x = x `mod` 7 == 0 || x `mod` 4 == 0

--22.3 - b
hasZeros :: Int->Int->Int->Bool
hasZeros a b c = b^2 - (4*a*c) < 0

--22.3 - v x^2 + (y-1)^2 = 25 
isInCircle :: Int->Int->Bool
isInCircle x y = x^2 + (y-1)^2 < 25

--22.3 - g 
isInDefinedCircle:: Int->Int->Int->Int->Int->Bool
isInDefinedCircle a b c d f = (a-c)^2 + (b-d)^2 < f^2

--22.3 - g
isInCircleAndThirdSquare :: Double->Double->Bool
isInCircleAndThirdSquare x y = (x<0 && y<0) && (x^2 + y^2 < 25)

--22.3 - d
isInRing:: Int->Int->Bool
isInRing x y = 
    let pow = x^2 + y^2
    in (25 <= pow) && (pow<=100)

--22.3 - e
isInLine :: Double->Bool
isInLine x = x>=0 && x<=1

--22.3 - zh
maxOfThree :: Int->Int->Int->Int->Bool
maxOfThree a b c x = x == max a (max b c)

--22.3 - z
notMaxOfThree :: Int->Int->Int->Int->Bool
notMaxOfThree a b c x = x /= max a (max b c)

--22.3 - i
notAPositive :: Int->Int->Int->Bool
notAPositive a b c = a < 0 && b < 0 && c < 0 

--22.3 - k
isSevenInNum :: Int -> Bool
isSevenInNum a = 
    (a `mod` 10 == 7) || (a `div` 10 `mod` 10 == 7) || (a `div` 100 `mod` 10 == 7)

--22.3 - l
areDifferent :: Int -> Bool
areDifferent a = 
    (a `mod` 10) /= (a `div` 10 `mod` 10) && 
    (a `mod` 10) /= (a `div` 100 `mod` 10) && 
    (a `div` 10 `mod` 10) /= (a `div` 100 `mod` 10)

--22.3 - m
areTwoEqual :: Int -> Bool
areTwoEqual a = 
    (a `mod` 10) == (a `div` 10 `mod` 10) ||
    (a `mod` 10) == (a `div` 100 `mod` 10) ||
    (a `div` 10 `mod` 10) == (a `div` 100 `mod` 10)

--22.3 - n
areIncreasing :: Int->Bool
areIncreasing a = 
     (a `mod` 10) > (a `div` 10 `mod` 10) &&
    (a `div` 10 `mod` 10) > (a `div` 100 `mod` 10)

areDecreasing :: Int->Bool
areDecreasing a = 
     (a `mod` 10) < (a `div` 10 `mod` 10) &&
    (a `div` 10 `mod` 10) < (a `div` 100 `mod` 10)

areIncrOrDecr:: Int -> Bool
areIncrOrDecr a = (areDecreasing a) || (areIncreasing a)

--22.3 - o 
areSymetric :: Int->Int->Bool
areSymetric a b = 
     (a `mod` 10) == (b `div` 100 `mod` 10) &&
     (a `div` 10 `mod` 10) ==  (b `div` 10 `mod` 10) &&
    (a `div` 100 `mod` 10) == (b `mod` 10)

--22.4
countDigits::Int->Int
countDigits 0 = 1  
countDigits n = countDigitsHelper n 0
  where
    countDigitsHelper 0 count = count
    countDigitsHelper n count = countDigitsHelper (n `div` 10) (count + 1)

--22.5
sumDigits::Int->Int
sumDigits 0 = 0  
sumDigits n = sumDigitsHelper n 0
  where
    sumDigitsHelper 0 sum = sum
    sumDigitsHelper n sum = sumDigitsHelper (n `div` 10) (sum + (n `mod` 10))

 --22.6 
powOfX::Int->Int->Int
powOfX x 0 = 1
powOfX x k = x * powOfX x (k-1)

--22.7 - a
isInNumber::Int->Int->Bool
isInNumber 0 0 = True
isInNumber 0 digit = False
isInNumber num digit = if digit == num `mod` 10 then True else isInNumber (num `div` 10) digit

--22.7 - b
countInNumber :: Int -> Int -> Int
countInNumber 0 0 = 1
countInNumber 0 digit = 0
countInNumber num digit = countInNumberHelper num 0
    where  
        countInNumberHelper 0 count = count
        countInNumberHelper n count =
            if (n `mod` 10) == digit then countInNumberHelper (n `div` 10) (count + 1) else countInNumberHelper (n `div` 10) count
--22.8
isLeapYear :: Int -> Bool
isLeapYear year =
    (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

--22.10
isPowOfK :: Int -> Int -> Bool
isPowOfK 0 k = False 
isPowOfK 1 k = True   
isPowOfK n k
    | n == k    = True  
    | n < k     = False 
    | otherwise = isPowOfK (n `div` k) k 

main :: IO ()
main = do
    print(isPowOfK 2048 3)
    