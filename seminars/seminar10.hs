main :: IO()
main = do

charTree = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []], Node 'c' []]

data Tree a = Nil | Node a [Tree a]
    deriving (Show)

empty = Nil
root = Node 1 []
tree = Node 1 [Node 2 [Node 5 [Node  9 [Node 14 [],
                                        Node 15 []],
                               Node 10 [Node 16 []],
                               Node 11 [],
                               Node 12 []],
                       Node 6 []],
               Node 3 [Node 7 [],
                       Node 8 [Node 13 []]],
               Node 4 []]

findDepth :: Tree a -> Int
findDepth Nil = 0 -- taka sme se razbrali
findDepth (Node _ []) = 1
findDepth (Node _ children) = 1 + (maximum $ map findDepth children)

foldTree :: (a -> [b] -> b) -> b -> Tree a -> b
foldTree _ null Nil = null
foldTree f null (Node value children) =
    f value (map (foldTree f null) children)

-- 1)
getKthLevel :: Int -> Tree a -> [a]
getKthLevel _ Nil = []
getKthLevel 1 (Node val _) = [val]
getKthLevel k (Node _ children) = concat $ map (getKthLevel (k-1)) children

-- 2)
containsWord :: [Char] -> Tree Char -> Bool
containsWord  ""   _ = True
containsWord   _ Nil = False
containsWord (s:ss) (Node letter children)
    | letter /= s    = False
    | True = foldr (||) (map (containsWord ss) children)

-- 3)
dfsTraversal :: Tree a -> [a]
dfsTraversal Nil = []
dfsTraversal (Node value children) = value : dfsChildren children
  where
    dfsChildren [] = []
    dfsChildren (c:cs) = dfsTraversal c ++ dfsChildren cs

-- 4)
getAllValues :: Tree Int -> [Int]
getAllValues Nil = []
getAllValues (Node value children) = value : concatMap getAllValues children

addBigger :: Tree Int -> Tree Int
addBigger tree = addBiggerHelper tree allValues
  where
    allValues = getAllValues tree

    addBiggerHelper :: Tree Int -> [Int] -> Tree Int
    addBiggerHelper Nil _ = Nil
    addBiggerHelper (Node value children) values =
      let biggerSum = sum [x | x <- values, x > value]
          newVal = value + biggerSum
          newChildren = map (`addBiggerHelper` values) children
      in Node newVal newChildren

-- 5)
treeEquals :: Eq a => Tree a -> Tree a -> Bool
treeEquals Nil Nil = True
treeEquals Nil   _ = False
treeEquals _   Nil = False
treeEquals tree1@(Node val1 children1) tree2@(Node val2 children2)
    | val1 /= val2                           = False
    | length children1 /= length children2   = False
    | True     = foldr (&&) True [treeEquals ch1 ch2 | (ch1, ch2) <- zip children1 children2] 