main :: IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]

type Tree = [(Int, [Int])]

t :: Tree
t = [(1, [2,3,4]), (2, [5,6]), (3, [7]), (4, [8,9]), (5, []), (6, [10]), (7, []), (8, []), (9, []), (10, [])]

findParent :: Int -> Tree -> Int
findParent node [] = 0
findParent node ((parent, children):tree)
    | any (\ child -> node == child) children = parent
    | otherwise = findParent node tree


findSiblings :: Int -> Tree -> [Int]
findSiblings node tree
    | parent == 0 = [] 
    | otherwise = concat [filter (\ x -> x /= node) siblings | (p, siblings) <- tree, p == parent]
    where
        parent = findParent node tree

findUncles :: Tree -> Int -> [Int]
findUncles tree node
    | parent == 0 = [] 
    | otherwise = findSiblings parent tree
    where
        parent = findParent node tree
