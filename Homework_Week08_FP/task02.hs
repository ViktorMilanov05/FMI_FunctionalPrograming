main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [4, 3, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]
    print $ listLeaves [(1, 2, 3), (4, 5, 6), (7, 8, 9)] == [2, 3, 5, 6, 8, 9]

type Node = (Int, Int, Int)

listLeaves :: [Node] -> [Int]
listLeaves binaryTree = filter (\x -> not (elem x parentList)) (concatMap (\(_, y, z) -> [y, z]) binaryTree)
 where
    parentList = map (\(x, _, _) -> x) binaryTree

