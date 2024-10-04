import Data.List

main :: IO()
main = do
    print $ maxSumSubT t1 == 5
    print $ maxSumSubT t2 == 2

maxSumSubT :: (Ord a, Num a) => BTree a -> a
maxSumSubT NullT = 0
maxSumSubT (Node value NullT NullT) = value
maxSumSubT (Node value left right) = maximum [(value + (maxSumSubT left) + (maxSumSubT right)), (maxSumSubT left), (maxSumSubT right)]


data BTree a = NullT | Node a (BTree a) (BTree a)


t1 :: BTree Int
t1 = Node 3 (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT)

t2 :: BTree Int
t2 = Node (-3) (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT)