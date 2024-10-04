main :: IO()
main = do
    print $ closestAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] == 21
    print $ closestAverage [(Temp 1 18), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 22), (Temp 31 24.5)] == 26 -- my test

average :: [Measuring] -> Float
average xs = sum floatValues / fromIntegral (length xs)
    where floatValues = map (\(Temp _ value) -> value) xs

closestAverage :: [Measuring] -> Int
closestAverage temps = getDay $ foldl1 closest temps
  where
    getDay (Temp day _) = day
    closest (Temp n1 t1) (Temp n2 t2)
      | abs (t1 - avg) < abs (t2 - avg) = Temp n1 t1
      | otherwise = Temp n2 t2
    avg = average temps

data Measuring = Temp Int Float
