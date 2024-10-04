numStepCombinations :: Int -> Integer --task 1
numStepCombinations n
    | n < 1     = error "n should be a natural number"
    | otherwise = helper n 0 1
    where
        helper :: Int -> Integer -> Integer -> Integer
        helper 0 _ b = b
        helper n a b = helper (n-1) b (a+b)

maxPersistenceMinSum :: Int -> Int -> Int --task 2
maxPersistenceMinSum start end = helper start end start
  where
    helper :: Int -> Int -> Int -> Int
    helper current end maxPersistenceNumber
     | current > end = maxPersistenceNumber
     | persistenceOfNumber current > persistenceOfNumber maxPersistenceNumber = helper (current + 1) end (current)
     | persistenceOfNumber current == persistenceOfNumber maxPersistenceNumber = helper (current + 1) end (minSumOfDigits current maxPersistenceNumber)
     | otherwise = helper (current + 1) end maxPersistenceNumber

persistenceOfNumber :: Int -> Int
persistenceOfNumber n = helper n 0
  where 
    helper :: Int -> Int -> Int
    helper n count
     | findMultiplyOfDigitsOfNumber n < 10 = count + 1
     | otherwise = helper (findMultiplyOfDigitsOfNumber n) (count + 1)

findMultiplyOfDigitsOfNumber :: Int -> Int
findMultiplyOfDigitsOfNumber 0 = 1
findMultiplyOfDigitsOfNumber n = mod n 10 * findMultiplyOfDigitsOfNumber (div n 10)

minSumOfDigits :: Int -> Int -> Int
minSumOfDigits a b
 | sumOfDigits a < sumOfDigits b = a
 | otherwise = b

sumOfDigits :: Int -> Int
sumOfDigits n = helper n 0
  where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper n result = helper (div n 10) (result + mod n 10)


-- Примери
main :: IO ()
main = do
    print $ numStepCombinations 2 == 2
    print $ numStepCombinations 3 == 3
    print $ numStepCombinations 100 == 573147844013817084101

    print $ maxPersistenceMinSum 273 392
    print $ maxPersistenceMinSum 1000 2000
    print $ maxPersistenceMinSum 55 105
    print $ maxPersistenceMinSum 195 756
    print $ maxPersistenceMinSum 2 85