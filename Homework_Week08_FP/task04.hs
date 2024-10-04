main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0
    print $ maxArea [Circle 40, Rectangle 25 10, Rectangle 30 25, Triangle 5.5 15 10, Cylinder 10 30] == Circle 40.0 -- my test

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show, Eq)

area :: Floating a => Shape a -> a
area (Circle r) = pi * (r * r)
area (Rectangle a b) = a * b
area (Triangle a b c) = sqrt (p * (p - a) * (p - b) * (p - c))
 where
    p = (a + b + c) / 2
area (Cylinder r h) = 2 * pi * r * h + 2 * pi * r * r

getAreas :: Floating a => [Shape a] -> [a]
getAreas = map area

maxArea :: (Floating a, Ord a) => [Shape a] -> Shape a
maxArea (x:xs) = foldl (\acc x -> if area x > area acc then x else acc) x xs
