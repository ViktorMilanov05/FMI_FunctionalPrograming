main :: IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364
    print $ getPalindromes 3 == 6 -- my test

getPalindromes :: Int -> Int
getPalindromes n = (minimum palindromes) + (maximum palindromes)
    where
        palindromes = filter isPalindrome (divisors n)


divisors :: Int -> [Int]
divisors n = [x | x <- [2..n], mod n x == 0]

isPalindrome :: Int -> Bool
isPalindrome n = reverse (show n) == show n

