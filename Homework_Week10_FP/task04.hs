main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False
    print $ isGraceful (Node 2 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]) == False -- my test

isGraceful :: Tree -> Bool
isGraceful (Node value nodes) = isAbsEven (Node value nodes) value
    where 
        isAbsEven Nil _ = True
        isAbsEven (Node value nodes) fatherValue
         | even (abs (fatherValue - value)) = all (\ node -> isAbsEven node value) nodes
         | otherwise = False

data Tree = Nil | Node Int [Tree]

t1 :: Tree
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 :: Tree
t2 = Node 7 [Node 9 [Node 5 [Nil],Node 2 [Nil]]]
