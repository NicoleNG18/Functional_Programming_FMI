main :: IO ()
main = do
    print $ expKthTerm 2 1
    print $ alternatingMap [(+1),(*2)] [1,2,3,4,5]

-- 1)
expKthTerm :: Int -> Double -> Double
expKthTerm k x = (sum . take k . map fst) $ iterate f (1, 0)
  where 
    f (curr, idx) = (curr * x / idx', idx')
      where 
        idx' = idx + 1

-- 2)
funnyComposition :: [Int -> Int] -> Int -> Int
funnyComposition funcs x 
    | even x    = evens x
    | otherwise = odds x
  where 
    evens = foldr (.) id $ map fst $ filter (\(_, idx) -> even idx) $ zip funcs [1..]
    odds  = foldr (.) id $ map fst $ filter (\(_, idx) -> odd idx)  $ zip funcs [1..]

-- 3)
calcCommulativeSums :: [Int] -> [Int]
calcCommulativeSums [] = []
calcCommulativeSums lst = drop 1 $ foldl f [0] lst
     where 
        f result x = result ++ [last result + x] 

-- 4)
alternatingMap :: [a -> b] -> [a] -> [b]
alternatingMap fns xs = zipWith ($) (cycle fns) xs 
                              -- (\fun x -> fun x)

-- 5)
sumDoubles :: [Int] -> [Int]
sumDoubles xs = zipWith (+) xs (tail xs)