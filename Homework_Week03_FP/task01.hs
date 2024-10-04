main :: IO()
main = do
    print $ removeFirstOccurrence 16366 5 == 16366
    print $ removeFirstOccurrence 110 1 == 10
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence n digit = helper n digit 0
  where 
    helper :: Int -> Int -> Int -> Int
    helper 0 _ _ = 0
    helper n digit count
     | mod n 10 == digit && count == 0 = helper (div n 10) digit (count + 1)
     | otherwise = (mod n 10) + 10 * helper (div n 10) digit count