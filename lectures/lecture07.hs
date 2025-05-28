import Data.List

data IntTree = Empty | Node Int IntTree IntTree
  deriving Show

mytree = Node 7
           (Node 30 
                 (Node 15 Empty Empty)
                 (Node 3 Empty Empty))
           (Node 77
                 Empty
                 (Node 15
                       (Node 70 Empty Empty)
                       Empty))


isMember :: IntTree -> Int -> Bool
isMember Empty _ = False
isMember (Node val leftChild rightChild) elem 
        | val == elem = True
        | otherwise =  isMember leftChild elem || isMember rightChild elem

sumElements :: IntTree -> Int
sumElements Empty = 0
sumElements (Node val leftCh rightCh) = val + sumElements leftCh + sumElements rightCh

maxElement :: IntTree -> Int
maxElement Empty = 0
maxElement (Node val leftCh rightCh) = max val (max (maxElement leftCh) (maxElement rightCh))  

data Direction = Left | Right deriving (Show, Eq)

myInsert :: IntTree -> [Direction] -> Int -> IntTree
myInsert Empty [] elem = Node elem Empty Empty
myInsert Empty (_:_) _ = Empty  
myInsert (Node val leftCh rightCh) (d:ds) elem
    | null ds && d == Main.Left  = Node val (Node elem Empty Empty) rightCh
    | null ds && d == Main.Right = Node val leftCh (Node elem Empty Empty)
    | d == Main.Left  = Node val (myInsert leftCh ds elem) rightCh
    | d == Main.Right = Node val leftCh (myInsert rightCh ds elem)
myInsert tree [] _ = tree  

main :: IO()
main = do
    print $ maxElement mytree    

