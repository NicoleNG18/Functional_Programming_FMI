-- Домашна работа по Функционално програмиране
-- Никол Георгиева - група 4 
main :: IO()
main = do

    let laptops = [ 
                Laptop "Dell"   2020 16 1500.0,
                Laptop "Apple"  2021 8  2000.0,
                Laptop "Lenovo" 2020 32 1800.0,
                Laptop "Asus"   2019 16 1200.0,
                Laptop "Dell"   2021 8  1000.0]

    let func = createAvgPriceFunc laptops
    print $ func ("Dell", 2019)

    print $ getTotalPrice laptops [1,2,3,4,5] [("Apple", 0.5), ("Asus", 0.1), ("Dell", 0.2), ("Lenovo", 0)]

    print $ findYear laptops

-- Задача 1. Списък от лаптопи се представя така:

-- Да се напише подходящ тип с конструктор Laptop.
data Laptop  = Laptop Brand Year RamGB Price
  deriving (Show, Eq)
type Brand = String
type Year = Int
type RamGB = Int
type Price = Double

-- Да се напише функция, която приема списък от лаптопи и връща функция на един параметър - 
-- наредена двойка от низ brand и число year, която при извикване връща средната цена на лаптопите, 
-- произведени от тази марка след посочената година.
-- func = createAvgPriceFunc laptops
-- func ("Dell", 2019) -- Очакван резултат: 1250.0

createAvgPriceFunc :: [Laptop] -> (String, Int) -> Double
createAvgPriceFunc laptops (br, y) = avg filteredPrices
  where
    filteredPrices = [price | Laptop brand year _ price <- laptops, brand == br, year > y]
    avg [] = 0
    avg ps = sum ps / fromIntegral (length ps)

-- Обяснение:
-- Създава списък filteredPrices, който съдържа само цените на лаптопите, 
-- които са от дадената марка и са произведени след зададената година.
-- Функцията avg изчислява средната стойност на цените в списъка:

-- Да се напише функция, която приема списък от лаптопи, списък от естествени числа(брой лаптопи от съответен вид, които ще се закупят) 
-- и списък от наредени двойки - (марка,надценка), който представя допълнителните разходи(в процент от цената)
-- при закупуване на лаптоп от конкретен производител.
-- getTotalPrice :: [Laptop] -> [Int] -> [(String, Double)] -> Double
-- getTotalPrice laptops [1,2,3,4,5] [("Apple", 0.5), ("Asus", 0.1), ("Dell", 0.2), ("Lenovo", 0)] -- Очакван резултат: 22480.0

getTotalPrice :: [Laptop] -> [Int] -> [(String, Double)] -> Double
getTotalPrice lst countList additionalMoney = 
  sum $ zipWith priceWithCount lst countList
  where
    getSurcharge _ [] = 0
    getSurcharge brand ((b, s):xs)
      | brand == b = s
      | otherwise  = getSurcharge brand xs
    priceWithCount (Laptop brand _ _ price) count = 
      price * (1 + getSurcharge brand additionalMoney) * fromIntegral count

-- Обяснение:
-- За всеки лаптоп и съответния брой в количествата пресмята цената с добавената надценка.
-- Умножава се цената с (1 + надценка), за да се включат допълнителните разходи.
-- Умножава се по броя на лаптопите от този вид.
-- След това всички тези суми се събират и връщат като обща цена.

-- Да се напише функция, която по подаден списък от лаптопи намира годината, през която "се е произвело" най-много RAM памет.
-- findYear :: [Laptop] -> Int
-- findYear laptops -- Очакван резултат: 2020

removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

maxBySnd :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxBySnd a b = if snd a >= snd b then a else b

findYear :: [Laptop] -> Int
findYear laptops = fst $ foldl1 maxBySnd totals
  where
    years = removeDuplicates (map (\(Laptop _ y _ _) -> y) laptops)
    totals = [(y, sum [ram | Laptop _ y' ram _ <- laptops, y' == y]) | y <- years]

-- Обяснение: 
-- removeDuplicates - премахва дублиращите се елементи от списък 
-- maxBySnd - сравнява две двойки (year, ramGB) и връща тази с по-голямото количество RAM
-- findYear - взима всички години от лаптопите, премахва дубликатите
-- За всяка уникална година събира общото количество RAM, произведено през нея .
-- Използва foldl1 maxBySnd да намери двойката с най-голямото общо количество RAM и връща годината от тази двойка