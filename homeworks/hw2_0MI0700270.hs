import Data.List

main :: IO()
main = do
    print $ warmerAfter [20,21,20,19,18,20,25,24,23,20,26] == [1,5,4,2,1,1,4,3,2,1,0]
    print $ warmerAfter [0,10,20,30] == [1,1,1,0]
    print $ warmerAfter [21,22,23] == [1,1,0]
    print $ warmerAfter [23,24,25,21,19,23,26,23] == [1,1,4,2,1,1,0,0]

    print $ ((setupRobots [0, 1] "LR") 3) == [-3, 4]
    print $ ((setupRobots [-2, 0, 2] "RLL") 2) == [-2, 0, 0]
    print $ ((setupRobots [-2, 0, 2] "RLL") 5) == [-5, -3, 3]
    print $ ((setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1) == [-1,-1,0,2,5,8,9,13,14]
    print $ ((setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3) == [-3,-2,0,1,7,7,10,12,15]
    print $ ((setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5) == [-5,-4,-2,3,5,9,10,12,17]
        
--  Task 1 

warmerAfter :: [Double] -> [Int]
warmerAfter ts = map (\index -> daysUntilWarmer index ts) [0..length ts - 1]

daysUntilWarmer :: Int -> [Double] -> Int
daysUntilWarmer index temps
    | index == length temps - 1 = 0
    | otherwise = findWarmer (index + 1) 1
    where
        findWarmer idx count
            | idx == length temps = 0
            | temps !! idx > (temps !! index) = count
            | otherwise = findWarmer (idx + 1) (count + 1)


--  Task 2

moveRobot :: Int -> Char -> [(Int, Char)] -> (Int, Char)
moveRobot robot direction robots
 | direction == 'L' && (elem ((robot - 1), 'R') robots) = ((robot), 'R')
 | direction == 'L' && (elem ((robot - 2), 'R') robots) = ((robot - 1), 'R')
 | direction == 'L' = ((robot - 1), direction)
 | direction == 'R' && (elem ((robot + 1), 'L') robots) = ((robot), 'L')
 | direction == 'R' && (elem ((robot + 2), 'L') robots) = ((robot + 1), 'L')
 | otherwise = ((robot + 1), direction)

movePositionsForOneSec :: [(Int, Char)] -> [(Int, Char)]
movePositionsForOneSec robotWithPosition = map (\ (robot, direction ) -> moveRobot robot direction robotWithPosition) robotWithPosition

movePositions :: [(Int, Char)] -> Int -> [Int]
movePositions robotWithPosition 0 = map (\ (robot, _) -> robot) robotWithPosition
movePositions robotWithPosition seconds = movePositions (movePositionsForOneSec robotWithPosition) (seconds - 1)

setupRobots :: [Int] -> String -> (Int -> [Int])
setupRobots xs ms = (\ t -> movePositions (zip xs ms) t)


