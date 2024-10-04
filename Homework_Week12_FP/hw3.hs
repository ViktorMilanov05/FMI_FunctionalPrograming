import Data.Char
import Data.List

main :: IO()
main = do
    print $ getParentSize fileSystem "i" == 584
    print $ getParentSize fileSystem "g" == 94853
    print $ getParentSize fileSystem "b.txt" == 48381165
    print $ getParentSize fileSystem "abc" == -1

-- task 1
findDirectoryByName :: Name -> [FileSystem] -> FileSystem
findDirectoryByName _ [] = Directory "" []
findDirectoryByName name ((Directory dirName directories):fsList)
 | name == dirName = (Directory dirName directories)
 | otherwise = findDirectoryByName name fsList
findDirectoryByName name (_:fsList) = findDirectoryByName name fsList

--concat logic start
changeChild :: FileSystem -> [FileSystem] -> [FileSystem]
changeChild (Directory dirName dirChildren) [] = [Directory dirName dirChildren]
changeChild (Directory dirName dirChildren) ((Directory name children):dirs)
  | dirName == name = (Directory dirName dirChildren) : dirs
  | otherwise = (Directory name children) : changeChild (Directory dirName dirChildren) dirs
changeChild dir (file:dirs) = file : changeChild dir dirs


replaceChild :: (FileSystem, Name) -> FileSystem -> (FileSystem, Name)
replaceChild ((Directory name children), parent) (Directory newName child) = 
  (Directory name (changeChild (Directory newName child) children), parent)


replaceParent :: Name -> [(FileSystem, Name)] -> FileSystem -> [(FileSystem, Name)]
replaceParent _ [] _ = []
replaceParent searchedName (((Directory name children), parent):xs) newChild
  | searchedName == name = (replaceChild ((Directory name children), parent) newChild) : xs
  | otherwise = ((Directory name children), parent) : replaceParent searchedName xs newChild

syncParent :: (FileSystem, Name) -> [(FileSystem, Name)] -> [(FileSystem, Name)]
syncParent _ [] = []
syncParent ((Directory dirName dirChild), dirParent) (((Directory name children), parent):xs)
 | dirParent == name = (replaceChild ((Directory name children), parent) (Directory dirName dirChild)) : xs
 | otherwise = ((Directory name children), parent) : syncParent ((Directory dirName dirChild), dirParent) xs

existsAtRoot :: String -> [(FileSystem, Name)] -> Bool
existsAtRoot dirName = any (\(Directory name _, parent) -> parent == "" && name == dirName)


concatTheFileSystems :: [(FileSystem, Name)] -> [(FileSystem, Name)] -> [(FileSystem, Name)]
concatTheFileSystems [] fullList = fullList
concatTheFileSystems (((Directory dirName children), parent):fss) fullList
  | parent == "" && not (existsAtRoot dirName fullList) = 
      concatTheFileSystems fss (((Directory dirName children), parent) : fullList)
  | parent == "" = 
      concatTheFileSystems fss fullList
  | otherwise = 
      concatTheFileSystems fss (replaceParent parent fullList (Directory dirName children))
--concat logic end

replaceLastTuple :: [(FileSystem, Name)] -> FileSystem -> [(FileSystem, Name)]
replaceLastTuple [] _ = error "Empty list"
replaceLastTuple [(fs, name)] newTuple = [(newTuple, name)]
replaceLastTuple (x:xs) newTuple = x : replaceLastTuple xs newTuple

getParent :: (FileSystem, Name) -> [(FileSystem, Name)] -> FileSystem
getParent ((Directory dirName children), dirParent) (((Directory name firstChildren), parent):fss)
  | dirParent == name = Directory name firstChildren
  | otherwise = getParent ((Directory dirName children), dirParent) fss

generateFileSystemWithFS :: FileSystem -> [Command] -> [(FileSystem, Name)] -> [(FileSystem, Name)]
generateFileSystemWithFS _ [] fss = concatTheFileSystems (reverse fss) (reverse fss)
generateFileSystemWithFS (Directory value children) (command:commands) fss
 | take 5 command == "$ cd " = let child = (findDirectoryByName (drop 5 command) children) in generateFileSystemWithFS child commands (fss ++ [(child, value)])
 | command == "$ cd .." = generateFileSystemWithFS (getParent (last fss) fss) commands (syncParent (last fss) (init fss))
 | take 4 command == "$ ls" = generateFileSystemWithFS (Directory value children) commands fss
 | take 4 command == "dir " = let addedDir = (Directory value (children ++ [(Directory (drop 4 command) [])])) in generateFileSystemWithFS addedDir commands (replaceLastTuple fss addedDir)
 | otherwise = let addedFile = (Directory value (children ++ [(File (dropWhile isSpace (dropWhile (\c -> not (isSpace c)) command)) (read (takeWhile isDigit command)))])) in generateFileSystemWithFS addedFile  commands (replaceLastTuple fss addedFile)

generateFileSystem :: [Command] -> [(FileSystem, Name)]
generateFileSystem (command:commands) = (generateFileSystemWithFS (Directory (drop 5 command) []) commands [((Directory (drop 5 command) []), "")])

-- task 2
getParentSize :: FileSystem -> Name -> Size
getParentSize fs filename
  | null sizes = -1
  | otherwise = minimum sizes
    where sizes = findFile fs filename []


findFile :: FileSystem -> Name -> [Size] -> [Size]
findFile (File name _) _ acc = acc
findFile (Directory dirName children) filename acc = results
  where 
  results = concatMap (\child -> findFileHelper filename child (sumDirectory (Directory dirName children) : acc)) children
  findFileHelper filename (File name size) acc
    | name == filename = acc
    | otherwise = []
  findFileHelper filename dir@(Directory _ children) acc =
    findFile dir filename acc

sumDirectory :: FileSystem -> Size
sumDirectory (File _ size) = size
sumDirectory (Directory _ children) = sum (map sumDirectory children)

type Command = String
type Size = Int
type Name = String

commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]

fileSystem :: FileSystem
fileSystem = Directory "/" [Directory "a" [Directory "e" [File "i" 584], File "f" 29116, File "g" 2557, File "h.lst" 62596], Directory "d" [File "d.ext" 5626152, File "d.log" 8033020, File "j" 4060174, File "k" 7214296], File "b.txt" 14848514, File "c.dat" 8504156]

data FileSystem = Directory Name [FileSystem] | File Name Size
 deriving (Eq, Show)