main :: IO()
main = do
    print $ sumDivisibleNumbers 50 10 5 == 290
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k
    | start > finish = sumDivisibleNumbers finish start k
    | start == finish && isDivisibleByK (sumOfDigits start) k = start
    | start == finish = 0
    | isDivisibleByK (sumOfDigits start) k = start + sumDivisibleNumbers (start + 1) finish k
    | otherwise = sumDivisibleNumbers (start + 1) finish k

sumOfDigits :: Int -> Int
sumOfDigits 0 = 0
sumOfDigits n = (mod n 10) + sumOfDigits (div n 10)

isDivisibleByK :: Int -> Int -> Bool
isDivisibleByK n k = mod n k == 0