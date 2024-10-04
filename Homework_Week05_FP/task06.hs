import Data.Char

main :: IO()
main = do
    print $ (pairCompose [(+1), (+2)]) 1 == 5 -- ((1 + 2) + 1) + 1
    print $ (pairCompose [(+1), (+2), (+3)]) 1 == 8
    print $ (pairCompose [(+1), (+2), (+3)]) 3 == 12 -- my test

pairCompose :: [(Int -> Int)] -> (Int -> Int)
pairCompose [] = \x -> x
pairCompose [f] = f
pairCompose (f1:f2:fs) = \x -> (f1 . f2 $ x) + pairCompose fs x