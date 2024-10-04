main :: IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecPM [5, 5, 5] == 15 --my test

    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6
    print $ mySumRecPM [5, 5, 5] == 15 --my test
 
    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6
    print $ mySumFunc [5, 5, 5] == 15 --my test

mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs
 | null xs = 0
 | otherwise = head xs + (mySumRecNonPM $ tail xs)

mySumRecPM :: [Int] -> Int
mySumRecPM [] = 0
mySumRecPM (x:xs) = x + mySumRecPM xs

mySumFunc :: [Int] -> Int
mySumFunc = sum 