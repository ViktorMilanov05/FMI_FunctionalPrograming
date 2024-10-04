import Data.List

main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))
    print $ convert (Node 1 Nil Nil) == (Node 1 Nil Nil) -- my test

data BTree a = Nil | Node a (BTree a) (BTree a)
  deriving (Show, Eq)

convert :: (Ord a, Num a) => BTree a -> BTree a
convert tree = convertUsingBfs tree (traverseBFS tree)
  where
    convertUsingBfs :: (Ord a, Num a) => BTree a -> [a] -> BTree a
    convertUsingBfs Nil _ = Nil
    convertUsingBfs (Node value left right) values = (Node (sum (filter (>=value) values)) (convertUsingBfs left values) (convertUsingBfs right values))

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

traverseBFS :: BTree a -> [a]
traverseBFS t = concat $ takeWhile (not . null) $ map (getLevel t) [0 .. ]


tree :: BTree Int
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))
