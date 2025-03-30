--0
removeConsecutive :: [Int] -> [Int]
removeConsecutive [] = []
removeConsecutive [x] = [x]
removeConsecutive (x:y:xs) = if x == y then removeConsecutive (y:xs) else x : removeConsecutive (y:xs)

--1
encode :: String -> [(Char, Int)]
encode [] = []
encode str = helper str []
    where 
        helper "" res = res
        helper (x:xs) [] = helper xs [(x,1)]
        helper (x:xs) ((sum, count) : res) = 
            if x == sum 
            then helper xs ((sum, count + 1) : res) 
            else helper xs ((x,1) : (sum, count) : res)

--2
getSquare :: [Int] -> [Int]
getSquare  lst = map square lst
    where square x = x * x

--3
div3notdiv7 :: [Int] -> [Int]
div3notdiv7 lst = filter cond lst
    where cond x = x `mod` 3 == 0 &&  x `mod` 7 /= 0

--4
takeAllButKths :: [a] -> Int -> [a]
takeAllButKths lst k = map fst $ filter cond (zip lst [1..])
    where 
        cond (_, index) = index `mod` k /= 0

--5
dotProduct :: [Double] -> [Double] -> Double
dotProduct xs ys = sum (zipWith (*) xs ys)

--6
split :: (a -> Bool) -> [a] -> ([a], [a])
split pred lst = (first,second)
    where 
        first = filter pred lst
        second = filter notpred lst
        notpred x = not (pred x)

--7 
triags :: [Double] -> [Double] -> [Double] -> [(Double, Double, Double)]
triags xs ys zs = [(x,y,z) | x <- xs, y <- ys, z <- zs, formTriag (x,y,z)]
    where 
        formTriag (a,b,c) = a > 0 && b>0 && c>0 && a< b+c && b<a+c && c<a+b



main :: IO()
main = do
    print(removeConsecutive [1,1,1,1,2,3,3,3])
    print(removeConsecutive [1,1,1,1])
    print(encode "abraaaacd")
    print(getSquare [1,2,3,4,5])
    print(takeAllButKths [1,2..20] 2)
    print(dotProduct [1,2,3] [1,1,1])
    print(split even [1,2,3,4,5,6,7])
    print(triags [1,2,3] [3,4,5] [5,6,7])
