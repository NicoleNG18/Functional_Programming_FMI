-- Домашна работа по Функционално програмиране
-- Никол Георгиева - група 4 
main :: IO()
main = do
    print $ areEqual tree tree
    print $ areEqual tree tree2

    print $ mirrorTree tree
    print $ areEqual expectedResult (mirrorTree tree)

    print $ stringifyTree tree3

    let tree4 = buildTree [(8,9),(5,3),(5,4),(1,5),(8,2),(1,8),(8,6)]

    print $ stringifyTree tree4

-- Задача 2. Нека е дадена следната дефиниция на тип двоично дърво с целочислени числа във възлите:

data BinTree = Nil | Node Int BinTree BinTree deriving(Show)

tree :: BinTree
tree = Node 5 (Node 4 Nil Nil)
              (Node 3 (Node 1 Nil Nil)
                      (Node 2 Nil Nil))

tree2 :: BinTree
tree2 = Node 5 (Node 4 (Node 8 Nil Nil) Nil)
               (Node 3 (Node 1 Nil Nil)
                       (Node 2 Nil Nil))

-- Да се напише предикат, който по подадени две двоични дървета връща истина точно когато те съвпадат, 
-- т.е. имат точно един и същ вид(еднаква дълбочина, еднаква разклоненост, еднакви елементи във възлите и т.н.).
-- areEqual :: BinTree -> BinTree -> Bool
-- areEqual tree tree -- Очакван резултат: True
-- areEqual tree tree2 -- Очакван резултат: False

areEqual :: BinTree -> BinTree -> Bool
areEqual Nil Nil = True
areEqual Nil _   = False
areEqual _   Nil = False
areEqual (Node val1 left1 right1) (Node val2 left2 right2) =
    val1 == val2 &&
    areEqual left1 left2 &&
    areEqual right1 right2

-- Обяснение: 
-- edge cases:
-- Ако и двете са празни, значи са еднакви - True.
-- Едно дърво е празно, другото не
-- Ако само едно от дърветата е празно, а другото не — False.

-- И двете дървета са Node. Проверяваме дали стойностите в корена са равни , дали лявото поддърво е равно
-- или дали дясното поддърво е равно
-- Ако всички тези условия са изпълнени, връщаме True, иначе False.

-- Да се напише функция, която по подадено дърво връща неговия огледален вариант(размяна на лявото и дясното дете).
-- mirrorTree :: BinTree -> BinTree

mirrorTree :: BinTree -> BinTree
mirrorTree Nil = Nil
mirrorTree (Node val left right) = Node val (mirrorTree right) (mirrorTree left)

expectedResult :: BinTree
expectedResult = Node 5 
                    (Node 3 
                        (Node 2 Nil Nil)
                        (Node 1 Nil Nil))
                    (Node 4 Nil Nil)

-- areEqual expectedResult (mirrorTree tree) -- Очакван резултат: True

-- Обяснение: 
-- Ако дървото е празно, връща Nil - еdge case
-- Ако дървото е възел, създава нов възел със същата стойност, като обръща местата на лявото и дясното поддърво

-- Нека е даден следният тип за произволно дърво:

data Tree = NIl | NOde Int [Tree] deriving(Show)

--  Да се напише функция, която по подадено произволно дърво го прeвръща в низ спрямо примера.
-- stringifyTree :: Tree -> String

tree3 :: Tree
tree3 = NOde 5 [NOde 6 [],
               NOde 7 [NOde 10 [], NOde 11 []],
               NOde 8 [NOde 9 []]]

-- stringifyTree tree -- Очакван резултат: [5,[[6],[7,[[10],[11]]],[8,[[9]]]]]

stringifyTree :: Tree -> String
stringifyTree NIl = "[]" 
stringifyTree (NOde val []) = "[" ++ show val ++ "]"
stringifyTree (NOde val children) =
  "[" ++ show val ++ "," ++ stringifyChildren children ++ "]"
  where
    stringifyChildren cs = "[" ++ inner cs ++ "]"
    inner [] = ""
    inner [t] = stringifyTree t
    inner (t:ts) = stringifyTree t ++ "," ++ inner ts

-- Обяснение: 
-- Ако дървото е празно, връща "[]".
-- Ако възелът няма деца, връща низа, съдържащ само стойността в квадратни скоби
-- Ако възелът има деца, връща низ [стойност, [низа на децата]]

-- Да се напише функция, която по списък от наредени двойки (баща,дете) конструира n-арно дърво. 
-- Приемете, че подаденият списък ще образува валидно дърво(няма да има цикли) 
-- и че няма повторение на стойности в резултатното дърво.
-- --           1
-- --        /     \
-- --       5       8
-- --     / |     / | \
-- --    3  4    9  2  6

-- buildTree :: [(Int, Int)] -> Tree

-- tree4 :: Tree
-- tree4 = buildTree [(8,9),(5,3),(5,4),(1,5),(8,2),(1,8),(8,6)]

-- stringifyTree tree4 -- Очакван резултат: [1,[[5,[[3],[4]]],[8,[[9],[2],[6]]]]]

buildTree :: [(Int, Int)] -> Tree
buildTree doubles = build root
  where
    parents = map fst doubles
    children = map snd doubles
    root = head [p | p <- parents, p `notElem` children]

    build :: Int -> Tree
    build node =
      let nodeChildren = [c | (p, c) <- doubles, p == node]
          childTrees = map build nodeChildren
      in NOde node childTrees

-- Обяснение: 
-- Намиране на корена на дървото
-- Коренът е този възел, който не се среща никъде като дете.
-- Използва се списък с всички бащи и всички деца и се избира се първият баща, който не е дете на никой.
-- За даден възел намираме всички негови деца. За всяко дете рекурсивно извикваме функцията build,
-- която изгражда поддървото с корен това дете.
-- След това създаваме възел NOde node childTrees, където childTrees са поддърветата на децата.