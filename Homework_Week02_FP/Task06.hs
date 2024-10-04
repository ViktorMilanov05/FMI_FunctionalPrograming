main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True
    print $ isInteresting 111 == True -- my test

isInteresting :: Int -> Bool
isInteresting x = mod x (sumDigits x) == 0

sumDigits :: Int -> Int
sumDigits x
 | x < 0 = error "The number has to be non-negative"
 | otherwise = helper x 0
   where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result + mod leftover 10)