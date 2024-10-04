import Data.List

main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ levelSum numberBTree 0 == 10 -- my test
    print $ cone numberBTree == True

cone :: BTree Int -> Bool
cone tree = (sort listOfTreeValues) == listOfTreeValues
  where
    listOfTreeValues = map (sum) (takeWhile (not . null) $ map (getLevel tree) [0 .. ])

levelSum :: BTree Int -> Int -> Int
levelSum tree  = sum . getLevel tree

getLevel :: BTree Int -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

data BTree a = Nil | Node a (BTree a) (BTree a)


numberBTree :: BTree Int
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))