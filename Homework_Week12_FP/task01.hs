main :: IO()
main = do
    print $ rangedSum firstTree 100 50 == 0 -- (L = 100, R = 50)
    print $ rangedSum firstTree 7 15 == 32 -- (L = 7, R = 15)
    print $ rangedSum firstTree 15 7 == 32 -- (L = 15, R = 7)
    print $ rangedSum secondTree 6 10 == 23 -- (L = 6, R = 10)
    print $ rangedSum secondTree 10 6 == 23 -- (L = 10, R = 6)
    print $ rangedSum firstTree 100 100 == 0 -- my test

rangedSum :: BTree Int -> Int -> Int -> Int
rangedSum Nil _ _ = 0 
rangedSum (Node value left right) l r
 | value >= (min l r) && value <= (max l r) = value + (rangedSum left l r) + (rangedSum right l r)
 | otherwise = (rangedSum left l r) + (rangedSum right l r)

data BTree a = Nil | Node a (BTree a) (BTree a)

firstTree :: BTree Int
firstTree = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))

secondTree :: BTree Int
secondTree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))