import Data.Char
import Data.List
import Data.Function
main :: IO()
main = do
   print $ myReverse 37563
   print $ reverseOrdSuff 37563


myReverse n = map (\ x -> digitToInt x) (show (helper n 0))
 where 
   helper 0 result = result
   helper n result =  helper (div n 10)(result * 10 + (mod n 10))

--reverseOrdSuff :: Int -> [Int]
reverseOrdSuff n = maximumBy (compare `on` length) (splitIntoSublists (map digitToInt (show n)))


splitIntoSublists [] = []
splitIntoSublists (x:xs) = reverse $ helper [x] xs
   where
      helper acc [] = [acc]
      helper acc (y:ys)
       | y < head acc = helper (y:acc) ys
       | otherwise = acc : helper [y] ys

zipWithNext (n:ns) = zip (n:ns) ns
--isInteresting :: Int -> Bool
isInteresting n = mod n sumOfDigits == 0
   where
      sumOfDigits = sum $ map (\ x -> digitToInt x) (show n)