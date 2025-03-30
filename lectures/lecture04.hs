myCount :: Int -> [Int] -> Int
myCount _ [] = 0
myCount a (x:xs)
    | a == x    = 1 + rest
    | otherwise = rest
    where rest = myCount a xs

sumElements :: [Int] -> Int
sumElements [] = 0
sumElements (x:xs) = x + (sumElements xs)

myPredic :: [Int] -> Bool
myPredic [] = False
myPredic [_] = False
myPredic (x:xs) =  if elem x xs then True else (myPredic xs)

countElements :: [Int] -> Int
countElements [] = 0
countElements (_:xs) = 1 + countElements xs

myPrefix :: Int -> [Int] -> [Int]
myPrefix _ [] = []
myPrefix a (x:xs) 
    | a > countElements (x:xs) || a == 0 = [] 
    | otherwise = x : myPrefix (a - 1) xs

myCommonPrefix :: [Int] -> [Int] -> [Int]
myCommonPrefix [] (x:xs) = []
myCommonPrefix (x:xs) [] = []
myCommonPrefix (x:xs) (y:ys) 
    | x/= y = []
    | otherwise = x : myCommonPrefix xs ys 

myMerge :: [Int] -> [Int] -> [Int]
myMerge [] l = l
myMerge l [] = l
myMerge (x:xs) (y:ys) 
    | x < y  =  x : (myMerge xs (y:ys))
    | x == y =  x : (myMerge xs ys)
    | x > y  =  y : (myMerge (x:xs) ys)     

main :: IO ()
main = do
    print (myMerge [1,3,7] [2,4,5,6])

