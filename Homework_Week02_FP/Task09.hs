main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14
    print $ everyOther 1234 == 31 -- my test

everyOther :: Int -> Int
everyOther n = helper n 0 1
  where
    helper :: Int -> Int -> Int -> Int
    helper 0 result _ = result
    helper number result count
     | odd count = helper (div number 10) result (count + 1)
     | otherwise = helper (div number 10) (result * 10 + mod number 10 ) (count + 1)