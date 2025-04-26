main :: IO()
main = do 

data WeekDay = Monday | Tuesday | Wednesday |
                Thursday | Friday | Saturday | Sunday
    deriving (Eq, Show)

-- 1)
isWorkingDay :: WeekDay -> Bool
isWorkingDay day = not (day `elem` [Saturday, Sunday])

-- 2)
data Genre = Fantasy | Novel | Lyric | SciFi | Romance
    deriving (Eq, Show)
type Title = String
type Author = String
type Year = Int
type Copies = Int
data Book = Book Title Author Genre Year
    deriving (Show,Eq)
type Library = [(Book, Copies)]

lib :: Library
lib = [(Book "Book2" "Author1" Novel 2000, 5),
       (Book "Book1" "Author2" SciFi 2001, 3),
       (Book "Book3" "Auhtor3" Novel 2002, 6)]

findTotalBooks :: Library -> Int
findTotalBooks = (sum . map snd)

takeAuthorsByGenre :: Genre -> Library -> [Author]
takeAuthorsByGenre gen =
    (map takeAuthor . filter (\((Book _ _ g _), _) -> g == gen))
        where
            takeAuthor ((Book _ author _ _), _) = author

getBookTitle :: Book -> String
getBookTitle (Book title _ _ _) = title

sortBooksAlphabetically :: Library -> Library
sortBooksAlphabetically [] = []
sortBooksAlphabetically (x:xs) = insertBook x (sortBooksAlphabetically xs)
  where
    getTitleFromLibrary (book, _) = getBookTitle book

    insertBook b [] = [b]
    insertBook b (by:bs)
      | getTitleFromLibrary b <= getTitleFromLibrary by = b : by : bs
      | otherwise = by : insertBook b bs

-- 3)
data Binom = Binom Int Int

calcBinom :: Binom -> Int
calcBinom (Binom n k) = fact n `div` (fact k * fact (n-k))
    where
        fact 0 = 1
        fact n = n * fact (n - 1)

-- 4)
data Nat = Zero | Succ Nat
    deriving (Eq, Show)

calcNat :: Nat -> Int
calcNat Zero = 0
calcNat (Succ x) = 1 + calcNat x

sumNat :: Nat -> Nat -> Nat
sumNat x y = helper x valY
    where
        valY = calcNat y

        helper x 0 = x
        helper x n = helper (Succ x) (n-1)

prodNat :: Nat -> Nat -> Nat
prodNat x y = helper x valY
    where
        valY = calcNat y
        helper x 0 = Zero
        helper x y = sumNat x (helper x (y-1))
        
-- 5)
data BTree = Nil | Node Int BTree BTree
    deriving Show

bTree :: BTree
bTree = (Node 5 (Node 3 (Node 1 Nil Nil)
                        (Node 2 Nil Nil))
                (Node 4 (Node 8 Nil Nil)
                        Nil))

depth :: BTree -> Int
depth Nil = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

nNodes :: BTree -> Int
nNodes Nil = 0
nNodes (Node _ left right) = 1 + (nNodes left) + (nNodes right)

nLeaves :: BTree -> Int
nLeaves Nil = 0
nLeaves (Node _ Nil Nil) = 1
nLeaves (Node _ left right) = (nLeaves left) + (nLeaves right)

mapTree :: (Int -> Int) -> BTree -> BTree
mapTree _                   Nil = Nil
mapTree f (Node val left right) = (Node (f val) leftt rightt)
    where
        leftt  = mapTree f  left
        rightt = mapTree f right

foldTree :: (Int -> a -> a -> a) -> a -> BTree -> a
foldTree _ defVal                   Nil = defVal
foldTree f defVal (Node val left right) = f val leftVal rightVal
    where
        leftVal  = foldTree f defVal  left
        rightVal = foldTree f defVal right

sumTree :: BTree -> Int
sumTree = foldTree (\ val left right -> val + left + right) 0

inOrderTraversal :: BTree -> [Int]  
inOrderTraversal Nil = []
inOrderTraversal (Node val left right) = leftt ++ [val] ++ rightt
    where
        leftt  = inOrderTraversal  left
        rightt = inOrderTraversal right

preOrderTraversal :: BTree -> [Int]  
preOrderTraversal Nil = []
preOrderTraversal (Node val left right) = [val] ++ leftt ++ rightt
    where
        leftt  = preOrderTraversal  left
        rightt = preOrderTraversal right

postOrderTraversal :: BTree -> [Int] 
postOrderTraversal Nil = []
postOrderTraversal (Node val left right) = leftt ++ rightt ++ [val]
    where
        leftt  = postOrderTraversal  left
        rightt = postOrderTraversal right

inOrderTraversal2 :: BTree -> [Int]
inOrderTraversal2 = foldTree (\val left right -> left ++ [val] ++ right) []