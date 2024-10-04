main :: IO()
main = do
    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53
    print $ sumPrimeDivs 1 == 0 -- my test

sumPrimeDivs :: Int -> Int
sumPrimeDivs n = helper 0 1
 where
    helper :: Int -> Int -> Int
    helper result divisor
     | divisor > n = result
     | mod n divisor == 0 && isPrime divisor = helper (result + divisor) $ divisor + 1
     | otherwise = helper result $ divisor + 1

isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
    helper divisor
     | divisor >= n = True
     | mod n divisor == 0 = False
     | otherwise = helper $ divisor + 1