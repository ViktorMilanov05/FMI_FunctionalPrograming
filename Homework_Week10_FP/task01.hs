main :: IO()
main = do
    print $ height numberBTree == 4
    print $ height charBTree == 3

    print $ average numberBTree == 16.22
    --print $ average charBTree -- should not work

    print $ sumLeaves numberBTree == 119
    --print $ sumLeaves charBTree -- shouldn't work

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False    
    print $ areEqual numberBTree Nil == False -- my test

    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))

    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))

mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node value left right) = (Node value (mirrorTree right) (mirrorTree left))

setLevels :: BTree a -> BTree (Int, a)
setLevels node = setByLevels node 0
    where
        setByLevels Nil _ = Nil
        setByLevels (Node value left right) level = (Node (level,value) (setByLevels left (level + 1)) (setByLevels right (level + 1)))  

areEqual :: (Eq a) => BTree a -> BTree a -> Bool
areEqual Nil Nil = True
areEqual (Node value Nil Nil) Nil = False
areEqual Nil (Node value Nil Nil) = False
areEqual Nil (Node value left right) = False
areEqual (Node value left right) Nil = False
areEqual (Node value1 left1 right1) (Node value2 left2 right2)
 | value1 == value2 = areEqual left1 left2 && areEqual right1 right2
 | otherwise = False

sumLeaves :: (Num a) => BTree a -> a
sumLeaves Nil = 0
sumLeaves (Node value Nil Nil) = value
sumLeaves (Node value left right) = sumLeaves left + sumLeaves right

roundTwoDig :: Double -> Double
roundTwoDig = (/ 100) . fromIntegral . round . (100 *)

size :: BTree a -> Int
size Nil = 0
size (Node _ left right) = 1 + size left + size right

sumTree :: (Num a) => BTree a -> a
sumTree Nil = 0
sumTree (Node value left right) = value + sumTree left + sumTree right

average :: (Integral a) => BTree a -> Double
average node = roundTwoDig $ fromIntegral (sumTree node) / fromIntegral (size node)

height :: BTree a -> Int
height Nil = 0
height (Node value left right) 
 | height left >= height right = 1 + height left
 | otherwise = 1 + height right


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))