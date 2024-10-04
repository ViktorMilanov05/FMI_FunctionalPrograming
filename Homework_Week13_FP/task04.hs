import Data.List

main :: IO()
main = do
    print $ isBoring t1 == False
    print $ isBoring t2 == True

isEqualToParent :: (Eq a) => NTree a -> a -> Bool
isEqualToParent NullT _ = True
isEqualToParent (Node value children) parent
 | parent == value = all (\ child -> isEqualToParent child value) children
 | otherwise = False

isBoring :: (Eq a) => NTree a -> Bool
isBoring (Node value children) = all (\ child -> isEqualToParent child value) children

data NTree a = NullT | Node a [(NTree a)]

t1 :: (Num a) => NTree a
t1 = Node 10 [Node 10 [Node 10 [NullT], Node 8 [Node 10 [NullT]], Node 2 [NullT]], Node 10 [Node 11 [NullT], Node 10 [NullT], Node 6 [NullT]]]

t2 :: NTree Char
t2 = Node 's' [Node 's' [NullT], Node 's' [NullT], Node 's' [NullT]]