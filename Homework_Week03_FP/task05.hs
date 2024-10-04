main :: IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51

p :: Int -> Int
p n = helper n 0
  where
    helper :: Int -> Int -> Int
    helper 1 result = result + 1
    helper current result = helper (current - 1) (result + current + (current - 1) * 2)