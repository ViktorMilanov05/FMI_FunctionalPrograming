main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 2
    print $ minDepthGreenNode (Node Green Empty Empty) == 1 -- my test

minDepthGreenNode :: Tree -> Int
minDepthGreenNode Empty = 0
minDepthGreenNode (Node value left right)
 | value == Green = 1
 | otherwise = 1 + min (minDepthGreenNode left) (minDepthGreenNode right) 


data Color = Red | Green | Blue
  deriving (Eq)
data Tree = Empty | Node Color Tree Tree

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)






