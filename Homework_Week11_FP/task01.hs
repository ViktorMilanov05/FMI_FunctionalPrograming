import Data.List

main :: IO()
main = do
    print $ (myPoly [2.7, 3.0 ..]) 2.2 3 == -0.4399999999999998
    print $ (myPoly [1, 2, 3]) 5 5 == 24.0 -- my test

myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs = (\x y -> product $ map (\(xi, i) -> x - xi) $ takeWhile (\(_, i) -> i /= y) $ zip xs [0..])