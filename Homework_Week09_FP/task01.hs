import Data.List

main :: IO()
main = do
    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0
    print $ (sumExpr (*3) [5, 3, 2, 6]) 5 == 12300 -- my test

sumExpr :: (Enum a, Num a) => (a -> a) -> [a] -> (a -> a)
sumExpr f ys = (\ x -> sum $ zipWith (\y xi -> y * f xi) ys [x^n | n <- [1..]])