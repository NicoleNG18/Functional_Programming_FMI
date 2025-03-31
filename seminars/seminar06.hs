transposeMatrix :: [[a]] -> [[a]]
transposeMatrix [] = []
transposeMatrix ([]:_) = []
transposeMatrix matrix = map head matrix : transposeMatrix (map tail matrix)

rotateMatrix :: [[a]] -> [[a]]
rotateMatrix [] = []
rotateMatrix ([]:_) = []
rotateMatrix lst = reverse (transposeMatrix lst)

spiral :: [[a]] -> [[a]]
spiral [] = []
spiral ([]:_) = []
spiral (x:xs) =  [x] ++ spiral (rotateMatrix xs)

calcPoly :: [Double] -> Double -> Double
calcPoly [] _ = 0
calcPoly (x:xs) el = x + el * calcPoly xs el

calcPolyFoldL :: [Double] -> Double -> Double
calcPolyFoldL coeffs x = foldl (\res coeff -> res * x + coeff) 0 coeffs

isLowerTriang :: (Num a, Eq a) => [[a]] -> Bool
isLowerTriang [] = True
isLowerTriang (x:xs) = all (== 0) (drop 1 x) && isLowerTriang xs

main :: IO()
main = do