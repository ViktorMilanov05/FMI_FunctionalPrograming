import Data.Char

main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 1 20 == [7,17] -- my test

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 1 20 == [7,17] -- my test

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [n | n <- [min x y..max x y], elem 7 (map digitToInt (show n)), isPrime n]

getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF x y = filter (\ n -> isPrime n && elem 7 (map digitToInt (show n))) [min x y..max x y]

isPrime :: Int -> Bool
isPrime n = [1, n] == filter (\ d -> mod n d == 0) [1 .. n]
