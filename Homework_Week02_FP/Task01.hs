main :: IO()
main = do

    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3
    print $ countDigitsIter 1 == 1 -- my test

    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3
    print $ countDigitsRec 1 == 1 -- my test

countDigitsIter :: Int -> Int 
countDigitsIter x
 | x < 0 = error "The number has to be non-negative"
 | otherwise = helper x 0
   where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result + 1)

countDigitsRec :: Int -> Int
countDigitsRec 0 = 0
countDigitsRec x
 | x < 0 = error "The number has to be non-negative"
 | otherwise = 1 + countDigitsRec (div x 10)