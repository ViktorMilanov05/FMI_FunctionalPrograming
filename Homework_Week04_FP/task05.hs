import Data.List

main :: IO()
main = do
    print $ isAscending 0 == True
    print $ isAscending 10 == False
    print $ isAscending 123 == True
    print $ isAscending 1233 == True
    print $ isAscending 12332 == False 
    print $ isAscending 12336 == True -- my test

isAscending :: Int -> Bool
isAscending n = sort (show n) == show n