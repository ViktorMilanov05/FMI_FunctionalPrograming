main :: IO()
main = do
    -- you may get slightly different results eg. -1.047619047619100 on test 4 <- not a problem
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 == -1.047619047619048
    print $ calcSeriesSum 1 4 == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
    print $ calcSeriesSum 1 6 == -1.0762718762718764


calcSeriesSum :: Double -> Double -> Double
calcSeriesSum x n = helper n 0
  where
    helper :: Double -> Double -> Double
    helper 0 result = result - 2
    helper n result = helper (n - 1) (result + calcCurrent n x)

calcCurrent :: Double -> Double -> Double
calcCurrent n x = ((-1)**(n+1)) * (((2**(n+1)) * (x**n)) / (calcDiv (n + 1)))

calcDiv :: Double -> Double
calcDiv n = helper n 1.0 1.0
  where
    helper :: Double -> Double -> Double -> Double
    helper 0 _ result = result
    helper count num result =  helper (count - 1) (num + 2) (result * num)