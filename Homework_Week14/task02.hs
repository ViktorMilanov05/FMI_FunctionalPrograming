import Data.List
import Data.Char

main :: IO()
main = do
    print $ prodEvens [1,2,3,4,5,6] == 15
    print $ prodEvens [7.66,7,7.99,7] == 61.2034

prodEvens :: (Num a) => [a] -> a
prodEvens = foldr (\(index, num) acc -> if even index then num * acc else acc) 1 . zip [0..]
