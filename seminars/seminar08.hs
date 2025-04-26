main :: IO()
main = do
    print $ treeEquals tree1 tree2
    print $ treeEquals tree1 tree1

data Tree a = Nil | Node a [Tree a] deriving Show

-- 1)
getKthLevel :: Tree a -> Int -> [a]
getKthLevel Nil _ = []
getKthLevel (Node value children) 0 = [value]
getKthLevel (Node _ children) k
    | k > 0 = getKthLevelChildren children (k-1)
    | otherwise = []
  where
    getKthLevelChildren [] _ = []
    getKthLevelChildren (c:cs) k = getKthLevel c k ++ getKthLevelChildren cs k

-- 2)
containsWord :: Tree Char -> String -> Bool
containsWord Nil _ = False
containsWord _ [] = True  
containsWord (Node value children) (x:xs)
    | value == x = 
        if null xs 
        then True
        else containsInChildren children xs
    | otherwise = containsInChildren children (x:xs)
  where
    containsInChildren [] _ = False
    containsInChildren (child:rest) word =
        containsWord child word || containsInChildren rest word

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
getAllValues (Node value children) = value : (sumChildren children)
  where
    sumChildren [] = []
    sumChildren (child:rest) = getAllValues child ++ sumChildren rest
addBigger :: Tree Int -> Tree Int
addBigger tree = addBiggerHelper tree (getAllValues tree)
  where
    addBiggerHelper Nil _ = Nil
    addBiggerHelper (Node value children) allValues = 
      let biggerSum = sum [x | x <- allValues, x > value]
      in Node (value + biggerSum) (map (\child -> addBiggerHelper child allValues) children)

-- 5)

tree1 :: Tree Int
tree1 = (Node 5 [(Node 3 [(Node 1 []),
                         (Node 2 [])]),
                (Node 4 [(Node 6 [])])
               ]
       )

tree2 :: Tree Int
tree2 = (Node 5 [(Node 3 [(Node 1 []),
                         (Node 2 [])])])

treeEquals :: Eq a => Tree a -> Tree a -> Bool
treeEquals Nil Nil = True
treeEquals Nil   _ = False
treeEquals _   Nil = False
treeEquals tree1@(Node val1 children1) tree2@(Node val2 children2)
    | val1 /= val2                           = False
    | length children1 /= length children2   = False
    | True     = foldr (&&) True [treeEquals ch1 ch2 | (ch1, ch2) <- zip children1 children2] 
