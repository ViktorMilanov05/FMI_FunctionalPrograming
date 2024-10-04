import Data.List

main :: IO()
main = do
    print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
    print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True
    print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False

getRepeatedLatesCount :: StudentRecord -> Int
getRepeatedLatesCount xss = maximum $ map (length) $ filter (\(x:xs) -> x == Late) $ group xss

canPass :: Critetion -> (StudentRecord -> Bool)
canPass (n,k) = (\ record -> (n >= (length (filter (==Absent) record))) && (k >= (getRepeatedLatesCount record)))

cP = canPass (1, 2)

type Misses = Int
type Lates = Int
type Critetion = (Misses, Lates)

data Attendance = Absent | Late | Present
    deriving(Show, Eq)
type StudentRecord = [Attendance]

