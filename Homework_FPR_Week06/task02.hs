type Cylinder = (Double, Double)

main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83]
    print $ getVolumes [(2, 6), (3, 1), (6, 2), (4, 3)] == [75.36,28.26,226.08,150.72] -- my test

getVolumes :: [Cylinder] -> [Double]
getVolumes cylinders = map (\(r, h) -> 3.14 * r^2 * h) cylinders
    