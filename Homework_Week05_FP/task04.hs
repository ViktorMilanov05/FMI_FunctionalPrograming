import Data.Char
import Data.List

main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69
    print $ specialSum 100 1000 == 29485 -- my test

specialSum :: Int -> Int -> Int
specialSum x y = sum $ filter (\ n -> elem 6 (map digitToInt (show n)) && mod n 4 == 1) [x..y]