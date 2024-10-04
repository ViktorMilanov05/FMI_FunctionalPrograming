main :: IO()
main = do
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter 5 == 5 -- my test

sumDigitsIter :: Int -> Int
sumDigitsIter x
 | x < 0 = error "The number has to be non-negative"
 | otherwise = helper x 0
   where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result + mod leftover 10)