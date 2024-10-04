main :: IO()
main = do
    print $ findSum 0 2 10  == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5  == 174 -- 26 + 50 + 98
    print $ findSum 10 5 6  == 575 -- my test

findSum :: Int -> Int -> Int -> Int
findSum a b n = helper a b (n - 1) 0 + helper a b (n - 2) 0 + helper a b (n - 3) 0
  where 
    helper :: Int -> Int -> Int -> Int -> Int
    helper a b 0 result = a + b + result
    helper a b power result = helper a b (power - 1) (result + b * 2 ^ power)
    
