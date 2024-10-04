import Data.List

main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1

applyN :: (a -> a) -> Int -> (a -> a)
applyN _ 0 = \x -> x
applyN f n = \x -> f $ applyN f (n-1) x