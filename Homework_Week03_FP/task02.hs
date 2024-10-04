main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

sortN :: Int -> Int
sortN n = helper n 0
  where 
    helper :: Int -> Int -> Int
    helper 0 result = result * 10 ^ countOfZeros n
    helper n result = helper (removeFirstOccurrence n (maxDigit n)) (result * 10 + maxDigit n)

countOfZeros :: Int -> Int
countOfZeros n = helper n 0
  where
    helper :: Int -> Int -> Int
    helper 0 count = count
    helper n count
     | mod n 10 == 0 = helper (div n 10) (count + 1)
     | otherwise = helper (div n 10) count

maxDigit :: Int -> Int
maxDigit n = helper n 0
  where
    helper :: Int -> Int -> Int
    helper 0 currentMax = currentMax
    helper n currentMax
     | currentMax < mod n 10 = helper (div n 10) (mod n 10)
     | otherwise = helper (div n 10) currentMax

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence n digit = helper n digit 0
  where 
    helper :: Int -> Int -> Int -> Int
    helper 0 _ _ = 0
    helper n digit count
     | mod n 10 == digit && count == 0 = helper (div n 10) digit (count + 1)
     | otherwise = (mod n 10) + 10 * helper (div n 10) digit count