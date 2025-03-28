import Data.Char (ord, chr)

xOrFunction :: Bool->Bool->Bool
xOrFunction x y = (x && not y) || (not x && y)

squareArea:: Int->Int
squareArea x = x^2

maxThree:: Int->Int->Int->Int
maxThree x y z = max x (max y z)

toUpper :: Char -> Char
offset:: Int
offset = (ord 'a') - (ord 'A')
toUpper x = if x >= 'a' && x <= 'z' then chr (ord x - offset) else x

fact :: Int -> Int
fact 0 = 1             
fact x =  x * fact (x - 1)  


main :: IO ()
main = do
  print (fact 5)


    