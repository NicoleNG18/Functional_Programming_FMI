magn7 :: [Int] -> Bool
magn7 [] = False
magn7 (7:_) = True
magn7 (_:_) = False

hasOne :: [Int] -> Bool
hasOne (_:[]) = True
hasOne _ = False

sumtail :: [Int] -> Int
sumtail (_:t) = sum t

myZip :: [Int]->[Int]->[(Int,Int)]
myZip [] _ = []
myZip _ [] = []
myZip (h1:t1) (h2:t2) = (h1,h2) : myZip t1 t2

shrink :: [(Int,Int)] -> [Int]
shrink [] = []
shrink (h1:t1) = fst(h1) : shrink (t1) 

shrinkTwo :: [Int] -> [Int]
shrinkTwo [] = []
shrinkTwo [_] = []
shrinkTwo (h1:h2:t2) = h1 + h2 : shrinkTwo t2 



main :: IO ()
main = do
--     print(zipFunc [1,2,3] [4,5,6])
    -- print(zip ["Ivan","Simona","Petra"] [1..])
    -- print(zipWith (+) [1,2,4] [3,5,6])
    -- print(zipWith div [10,20,40] [2,10,4])
    -- print(zipWith (:) [1,2,4,5][[2,3],[4,5],[6,7],[8,9]])
    -- print(zipWith take [1,2,3,4] [[1..],[3..],[7..],[0..]])
    -- print(zip "Hello" [1..])
    -- print(zipWith (!!) ["Ivan","Simona","Petra","Ivena"] [0,0..])
    -- print(fib 4)
    print(shrinkTwo [1,2,3,4,5,6,7])