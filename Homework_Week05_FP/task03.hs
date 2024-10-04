main :: IO()
main = do
    print $ isArithmetic [3] == True
    print $ isArithmetic [3, 5] == True
    print $ isArithmetic [1, 2, 3, 4, 5] == True
    print $ isArithmetic [3, 5, 7, 9, 11] == True
    print $ isArithmetic [3, 5, 8, 9, 11] == False
    print $ isArithmetic [3, 5, 9, 9, 11] == False
    print $ isArithmetic [2, 4, 6, 8, 10, 12] == True -- my test

isArithmetic :: [Int] -> Bool
isArithmetic xs = all (\i -> (xs !! (i + 1)) - (xs !! i) == (xs !! 1) - (xs !! 0)) [0..length xs - 2]
