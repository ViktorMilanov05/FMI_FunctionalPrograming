
main :: IO()
main = do
    print $ maxDepthBlueNode colorTree == 2
    print $ maxDepthBlueNode (Node Blue Empty Empty) == 1 -- my test

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode Empty = 0
maxDepthBlueNode (Node value left right)
    | value == Blue = 1 + max (maxDepthBlueNode left) (maxDepthBlueNode right)
    | otherwise = max (maxDepthBlueNode left) (maxDepthBlueNode right)


data Color = Red | Green | Blue
  deriving (Eq)
data Tree = Empty | Node Color Tree Tree

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)