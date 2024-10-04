import Data.Char

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462
    print $ sumSpecialPrimes 0 10 == 0 -- my test

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n $ filter (\ x -> isPrime x && isContainsDigit d x) [1..]

isContainsDigit :: Int -> Int -> Bool
isContainsDigit digit = elem digit . map digitToInt . show
-- | number < 10 = number == digit
-- | mod number 10 == digit = True
-- | otherwise = isContainsDigit digit $ div number 10

isPrime :: Int -> Bool
isPrime n = [1, n] == filter (\ d -> mod n d == 0) [1 .. n]