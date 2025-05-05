-- 25.1
-- Направете и тествайте собствена реализация на функциите map, filter
-- и fold(l,r,l1,r1).
myMap :: (a -> a) -> [a] -> [a]
myMap _ [] = []
myMap func (x:xs) = (func x) : myMap func xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []  
myFilter func (x:xs)
  | func x    = x : myFilter func xs  
  | otherwise = myFilter func xs    

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr func acc (x:xs) = func x (myFoldr func acc xs)

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [] = []
myFoldr1 _ [x] = x               
myFoldr1 func (x:xs) = func x (myFoldr1 func xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl func acc (x:xs) = myFoldl func (func acc x) xs

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 _ [] = []
myFoldl1 _ [x] = x
myFoldl1 func (x:y:ys) = myFoldl1 func (func x y : ys)

-- 25.2 
--  Нека е даден списък l::[(Int,Int,Int)] с тройки (ai,bi,ci). С помощта
-- на map, fold и filter да се намерят:
-- а) Списъка от сумите на елементите на тройките [(ai + bi + ci)]
-- б) Тройка от сумите на отделните компоненти на елемнтите на l,
-- ( ai, bi, ci)
-- в) Броя на тройките, за които ai + bi >ci
-- г) Дали има поне една тройка, за която ai = bi = ci (True или False)

-- a)
sumElements :: [(Int, Int, Int)] -> [Int]
sumElements = map (\(a,b,c) -> a + b + c)

-- b)
tripple :: [(Int, Int, Int)] -> (Int, Int, Int)
tripple lst = (aSum, bSum, cSum)
  where
    aSum = sum (map (\(a,_,_) -> a) lst)
    bSum = sum (map (\(_,b,_) -> b) lst)
    cSum = sum (map (\(_,_,c) -> c) lst)

-- c)
countFiltered :: [(Int, Int, Int)] -> Int
countFiltered = length . filter (\(a,b,c) -> a + b > c)

-- d) 
hasEquals :: [(Int, Int, Int)] -> Bool
hasEquals lst = length (filter (\(a, b, c) -> a == b && b == c) lst) > 0

--25.3 
-- За списък от числа L да се намери списък с само с тези числа, които
-- съвпадат с поредния си номер в L. Например [1,5,3,4,2] →[1,3,4].
isEqualWithIndex :: [Int] -> [Int]
isEqualWithIndex lst = isEqualWithIndexHelper (zip lst [0..])
  where 
    isEqualWithIndexHelper [] = []  
    isEqualWithIndexHelper ((fx, sx):xs)
      | fx == sx = fx : isEqualWithIndexHelper xs  
      | otherwise = isEqualWithIndexHelper xs 

--25.4. 
-- За списък от числа L да се намери списък със сумите на всички двойки
-- последователни елементи на L. Например [1,5,3,4,2] →[6,8,7,6]. Упът-
-- ване: Използвайте zip.
pairSums :: [Int] -> [Int]
pairSums xs = [x + y | (x, y) <- zip xs (tail xs)]

-- 25.5. 
-- С помощта на zipWith да се дефинира функция sums :: [Int] -> [Int],
-- която по списък от числа L= l1,l2,l3,... намира списъка S= l1,(l1 +
-- l2),(l1 + l2 + l3),....
sums :: [Int] -> [Int]
sums []     = []
sums (x:xs) = x : zipWith (+) (sums (x:xs)) (tail (sums (x:xs)))


-- 25.6.
--  Даседефинирафункцияseparate :: (a->Bool) -> [a] -> ([a],[a]),
-- която по предикат pи списък l връща двойката (pref,suf). pref е най-
-- дългият възможен префикс, такъв че всички негови елементи увовлет-
-- воряват p. suf е останалата част от списъка l. Например, separate even
-- [2,4,6,7,8,10] -> ([2,4,6],[7,8,10]). Забележка: вижте функция-
-- та break в Prelude.
separate :: (a -> Bool) -> [a] -> ([a], [a])
separate p lst = (pref, suf)
  where
    pref = [x | x <- lst, p x]  
    suf  = drop (length pref) lst

-- 25.7.
--  Да се дефинира функция split :: (a->Bool) -> [a] -> [[a]], получ-
-- ваваща предикат p и списък l. Елементите на l, удовлетворяващи p, се
-- считат за “разделители” в l и списъкът се разделя на части, обособени
-- от тези разделители. Например:
-- s p l i t (==’,’) "part1 , part␣2,part3"
-- − > ["part1" ,"part␣2" ,"part3"]
split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p = foldr (\x acc -> if p x then [] : acc else (x : head acc) : tail acc) [[]]

--25.9
--  С помощта на foldr да се дефинира функция, която проверява да-
-- ли дали даден списък от числа l::[Int] е нареден във възходящ ред.
-- Упътване: Използвайте двойка (Bool,Int) за акумулатор.
isAscending :: [Int] -> Bool
isAscending = fst . foldr check (True, minBound) 

check :: Int -> (Bool, Int) -> (Bool, Int)
check x (isAsc, prev) 
  | x >= prev = (isAsc, x)  
  | otherwise = (False, x)  










