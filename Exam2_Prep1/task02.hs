import Data.List
import Data.Char

main :: IO()
main = do
    print $ solve ["abode","ABc","xyzD"]  == [4,3,1]
    print $ solve ["abide","ABc","xyz"] == [4,3,0]
    print $ solve ["IAMDEFANDJKL","thedefgh","xyzDEFghijabc"] == [6,5,7]
    print $ solve ["encode","abc","xyzD","ABmD"] == [1, 3, 1, 3]

solve :: [String] -> [Int]
solve = map (length) . map (\ letters -> filter (\ letter -> elem letter (zip [1..] ['a'..'z'])) letters) . map (\word -> zip [1..] (map (toLower) word))


