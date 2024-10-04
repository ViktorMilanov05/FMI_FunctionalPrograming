import Data.List

main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"
    print $ (repeater "Hello") 2 "Viktor" == "HelloViktorHello" -- my test

repeater :: String -> (Num a, Integral a) => (Int -> String -> String)
repeater str = \count glue -> intercalate glue $ genericReplicate count str