import Data.List

main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False
    print $ ordered (Node (3, 10) Nil Nil) == True -- my test

traverseDFS :: Tree -> [Int]
traverseDFS Nil = []
traverseDFS (Node (start, end) left right) = traverseDFS left ++ [end - start] ++ traverseDFS right

ordered :: Tree -> Bool
ordered tree = let nodes = traverseDFS tree
 in sort nodes == nodes



data Tree = Nil | Node (Int, Int) Tree Tree

t1 :: Tree
t1 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (4, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

t2 :: Tree
t2 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (7, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))