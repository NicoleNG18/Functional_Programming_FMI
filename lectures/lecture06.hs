data Gender = Male | Female
    deriving (Show, Eq)

data Person = Person 
  { name     :: String
  , birthday :: (Int, Int, Int)
  , gender   :: Gender
  , height   :: Float 
  } deriving (Show, Eq)

intersection l1 l2 = filter (\x -> x `elem` l1 && x `elem` l2)
intersection l1 l2 = filter (\x -> elem l2) l1  

myunion l1 l2 = l1 ++ filter (\e -> not (elem e l1)) l2

qsort (p:l) = qsort left ++ p ++ qsort right
    where left = filter (< p) l
           right = filter (>= p) l

mydiff l1 l2 = filter (\e -> not(elem e l2)) l1

hasEvenEl l = elem True (map even l)

h = map height people 
(sum h) / fromIntegral (length h)

gtf x y l = length $ filter (\f -> f x > y) l

countelem e l = foldr 0 (\x sum -> sum + (if x = e then 1 else 0)) l
myelem e l = foldr (\x sum ->  (x==e) || sum) False l
mymap f l = foldr (\x accum -> (f x) : accum) [] l
myfilter p l = foldr (\x accum -> if px then x : accum else accum) [] l
myreverse l = foldl (\accum x -> x : accum) [] l

next (x,y) = (if x <= y then x+1 else x, if x<=y then 0 else y+1)

main :: IO ()
main = do
        let people = 
                    [ Person { name = "Ivan Petrov", birthday = (1998, 1, 1), gender = Male, height = 185 }
                    , Person { name = "Petar Ivanov", birthday = (2002, 5, 1), gender = Male, height = 179 }
                    , Person { name = "Maria Ivanova", birthday = (2003, 6, 11), gender = Female, height = 182 }
                    , Person { name = "Ivana Petrova", birthday = (2003, 1, 1), gender = Female, height = 193 }
                    ]
  

