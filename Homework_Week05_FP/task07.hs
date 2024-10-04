main :: IO()
main = do
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 == 3
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 == 9
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 == 16
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30
    print $ (switchSum (\x -> x + 5) (\x -> x * 3) 3) 5 == 75

switchSum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
switchSum f g n = \x -> f x + helper g f (f x) (n - 1)
    where helper :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
          helper f1 f2 val 0 = 0
          helper f1 f2 val n = f1 val + helper f2 f1 (f1 val) (n - 1)