--26.1
-- Да се дефинира функция caseof :: [a->Bool] -> [a->b] -> a->b. Фун-
-- кцията приема k на брой предиката над a и k на брой функции a →b.
-- Функцията caseof връща функция, която приема елемент x от тип a и
-- връща резултата от първата функция, за която предикатът е изпълнен
-- за x. Ако няма такава функция, функцията връща резултата от послед-
-- ната функция в списъка. Например, функцията:
-- myf = caseof [even , odd ] [(\x− > x+1), (\x− > x−1)]
-- ще увеличава с единица четните числа и ще намалява с единица нечет-
-- ните числа.
caseof :: [a -> Bool] -> [a -> b] -> a -> b
caseof preds funcs x = chosenF x
  where
    defaultF = last funcs
    pairs    = zip preds funcs
    chosenF  = foldr (\(p, f) acc -> if p x then f else acc) defaultF pairs

-- 26.2.
--  Да се дефинира функция createfn :: [(a,b)]->a->b, която по даден
-- списък от двойки (ai,bi), връща функция f : a→b дефинирана за всич-
-- ки ai, така че f(ai) = bi. Например:
-- createfn [(1 ,2) ,(2 ,4) ,(3 ,6) ] 2-> 4
createfn :: Eq a => [(a, b)] -> a -> b
createfn lst = \x -> snd . head . filter (\(f, _) -> f == x) $ lst

main :: IO ()
main = do
    print (createfn testData2 "banana")