main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134
    print $ removeD 7 7777 == 0 -- my test

removeD :: Int -> Int -> Int
removeD d n = reverseNum $ helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper number result
     | mod number 10 == d = helper (div number 10) result
     | otherwise = helper (div number 10) (result * 10 + mod number 10)

reverseNum :: Int -> Int
reverseNum x = helper x 0
  where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper x result = helper (div x 10) (result * 10 + mod x 10)
