main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11
    print $ countPalindromes 1 9  == 7 -- my test

countPalindromes :: Int -> Int -> Int
countPalindromes x y = helper (min (x + 1) (y + 1)) (max x y) 0
  where 
    helper :: Int -> Int -> Int -> Int
    helper start end count
     | start == end = count
     | reverseNum start == start = helper (start + 1) end (count + 1)
     | otherwise = helper (start + 1) end count

reverseNum :: Int -> Int
reverseNum x = helper x 0
  where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper x result = helper (div x 10) (result * 10 + mod x 10)
