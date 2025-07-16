-- Домашна работа по Функционално програмиране
-- Никол Георгиева - група 4 
main :: IO()
main = do
    print $ calcBoolExpr "T & F" []
    print $ calcBoolExpr "T & F" [('x', False)]
    print $ calcBoolExpr "! T & T | x" [('x', True)] 
    print $ calcBoolExpr "! x & 0 | y & ! 3 | F" [('x', True), ('y', False)] 
    print $ calcBoolExpr "T | ! 0 & x | y" [('x', False), ('y', False)]
    print $ calcBoolExpr "! x & ! 1 | z & y | ! 0" [('x', False), ('y', True), ('z', False)]
    
-- Задача 4. Тази задача разглежда опростен калкулатор на логически изрази. 
-- Нека е даден низ, описващ логически израз, който съдържа единствено части от вида:
-- - символи 'T' и 'F', които описват съответно истина и лъжа;
-- - символите '&', '|', '!', които съответстват на логическо и, логическо или и отрицание;
-- - естествени числа(последователности от цифри без интервали между тях);
-- - идентификатори на променливи - малка латинска буква;
-- - интервали.  

-- Между всяка част в израза има поне един интервал. Да се напише функция, която получава низ от гореописания вид и списък от 
-- наредени двойки, първият елемент на всяка от които е идентификатор от израза, а вторият - логическа стойност, с която да се замести.
--  Единствено числото 0 се оценява с лъжа, а всяко друго естествено число - с истина. Функцията връща стойността на израза. 
--  Не е необходимо да валидирате входните данни.
-- calcBoolExpr :: String -> [(Char, Bool)] -> Bool
-- Припомняне: Отрицанието се отнася за непосредствено следващата го променлива или константа, 
-- а приоритетът на логическото И е по-висок от този на логиеското ИЛИ.

data BoolExpr
    = Val Bool
    | Not BoolExpr
    | And BoolExpr BoolExpr
    | Or BoolExpr BoolExpr
    deriving (Show)

-- Обяснение: 
-- Val Bool - булева стойност
-- Not BoolExpr - отрицание 
-- And BoolExpr BoolExpr - И
-- Or BoolExpr BoolExpr - ИЛИ 

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- Обяснение: 
-- isDigit проверява дали даден символ е цифра.

isAllDigits :: String -> Bool
isAllDigits [] = False
isAllDigits str = check str
  where
    check [] = True
    check (x:xs) = isDigit x && check xs

-- Обяснение: 
-- isAllDigits проверява дали низът съдържа само цифри 

isBoolFound :: Char -> [(Char, Bool)] -> Bool
isBoolFound _ [] = False 
isBoolFound c ((symbol,boolVal):xs)
  | c == symbol    = boolVal
  | otherwise = isBoolFound c xs

-- Обяснение: 
-- isBoolFound търси стойността на променлива в подадения списък 

charToBool :: String -> [(Char, Bool)] -> Bool
charToBool "T" _ = True
charToBool "F" _ = False
charToBool symbol elements
    | isAllDigits symbol = symbol /= "0"
    | length symbol == 1 && head symbol >= 'a' && head symbol <= 'z' = isBoolFound (head symbol) elements
    | otherwise = False

-- Обяснение:
-- charToBool превръща символ или число в булева стойност според правилата:
-- T -> True
-- F -> False
-- число 0 -> False
-- всички други числа -> True
-- променлива -> търси стойността ѝ в списъка
-- други случаи -> False   

-- приема низ и среда с променливи, парсва израза и връща неговата булева стойност
calcBoolExpr :: String -> [(Char, Bool)] -> Bool
calcBoolExpr input elements = evaluateExpression (parseOr (words input))
-- Изразът се разбива на думи с функцията words
-- Приоритетът на операциите е (! > & > |)
  where
    parseOr tokens = parseOr' (parseAnd left) rest
      where (left, rest) = splitBy "|" tokens

    parseOr' left [] = left
    parseOr' left right = Or left (parseOr right)
-- parseOr парсва изрази, разделени с '|', използвайки parseAnd
    parseAnd tokens = parseAnd' (parseNot left) rest
      where (left, rest) = splitBy "&" tokens

    parseAnd' left [] = left
    parseAnd' left right = And left (parseAnd right)
-- parseAnd парсва изрази, разделени с '&', използвайки parseNot
    parseNot ("!":x:xs) = Not (parseNot [x])
    parseNot (x:xs)     = Val (charToBool x elements)
    parseNot []         = Val False
-- parseNot обработва отрицанието и превръща думите в BoolExpr 

-- evaluateExpression рекурсивно пресмята стойността на BoolExpr дървото
-- връща булева стойност според логическите правила
    evaluateExpression (Val b) = b
    evaluateExpression (Not x) = not (evaluateExpression x)
    evaluateExpression (And a b) = evaluateExpression a && evaluateExpression b
    evaluateExpression (Or a b) = evaluateExpression a || evaluateExpression b

-- за разделяне на израза по оператори
    splitBy _ [] = ([], [])
    splitBy op (x:xs)
      | x == op  = ([], xs)
      | otherwise =
          let (before, after) = splitBy op xs
          in (x : before, after)