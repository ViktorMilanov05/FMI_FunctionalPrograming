main :: IO()
main = do
    print $ sumUnevenLC 5 50 == 621
    print $ sumUnevenLC 50 1 == 625
    print $ sumUnevenLC 564 565 == 565
    print $ sumUnevenLC 0 0 == 0 -- my test
    
    print $ sumUnevenHOF 5 50 == 621
    print $ sumUnevenHOF 50 1 == 625
    print $ sumUnevenHOF 0 0 == 0 -- my test

sumUnevenLC :: Int -> Int -> Int
sumUnevenLC start end = sum $ [ x | x <- [min start end .. max start end], odd x]

sumUnevenHOF :: Int -> Int -> Int
sumUnevenHOF start end = sum $ filter (\ x -> odd x) [min start end .. max start end]