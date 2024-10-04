main :: IO()
main = do
    print $ countOccurrences 121 1 == 2
    print $ countOccurrences 222 1 == 0
    print $ countOccurrences 100 0 == 2
    print $ countOccurrences 0 0 == 1  
    print $ countOccurrences 452633 3 == 2 -- my test 

countOccurrences :: Int -> Int -> Int
countOccurrences x y = helper x 0
 where
    helper :: Int -> Int -> Int
    helper x result
     | x < 10 && y == x = result + 1
     | x < 10 = result
     | mod x 10 == y = helper (div x 10) (result + 1)
     | otherwise = helper (div x 10) result
