import Data.List
import Data.List.Split
import Text.Printf
data File = File String Integer deriving (Show)
data Folder = Folder String [File] [Folder] deriving (Show)

main :: IO ()
main = do
  input <- readFile "input"
  let (root, path) = parseLines (Folder "" [] [], []) (lines input)
  let sizes = allSize root
  printf "part 1: %d\n" (partialSum sizes 100000)
  let rootSize = folderSize root
  let freeSpace = 70000000 - rootSize
  let neededSpace = 30000000 - freeSpace
  let sortedSizes = sort sizes
  printf "part 2: %d\n" (findThresh sortedSizes neededSpace)

findThresh :: [Integer] -> Integer  -> Integer
findThresh (i:is) thresh = if i >= thresh 
  then i 
  else (findThresh is thresh)
findThresh [] thresh = 0

newFolder :: String -> Folder
newFolder name = Folder name [] []

newFile :: String -> File
newFile line = do
  let l = splitOn " " line
  let size = read (head l) :: Integer
  let name = head (tail l)
  File name size

parseLines :: (Folder, [String]) -> [String] -> (Folder, [String])
parseLines folder (l:ls) = parseLines (parse folder l) ls
parseLines folder [] = folder

parse :: (Folder, [String]) -> String -> (Folder, [String])
parse folder "" = folder
parse folder "$ ls" = folder
parse folder ('$':' ':'c':'d':' ':target) = cd folder target
parse (root, path) ('d':'i':'r':' ':name) = (put (root, path) (newFolder name), path)
parse (root, path) line = (putFile (root, path) (newFile line), path)

cd :: (Folder, [String]) -> String -> (Folder, [String])
cd (folder, _) "/" = (folder, [])
cd (folder, cursor) ".." = (folder, take ((length cursor)-1) cursor)
cd (folder, cursor) target = (folder, cursor ++ [target])

upsert :: [Folder] -> Folder -> [Folder]
upsert [] new = [new]
upsert (old:folders) new = do
  let (Folder oldName _ _) = old
  let (Folder newName _ _) = new
  if oldName == newName
  then
    new:folders
  else
    old:(upsert folders new)

pick :: [Folder] -> String -> Folder
pick [] _ = Folder "error" [] []
pick (folder:folders) path = do
  let (Folder name  _  _) = folder
  if name == path
    then
      folder 
    else
      pick folders path

put :: (Folder, [String]) -> Folder -> Folder
put ((Folder name files folders), []) folder = Folder name files (upsert folders folder)
put ((Folder name files folders), (parent:children)) folder = do
  let old = pick folders parent
  Folder name files (upsert folders (put (old, children) folder))

putFile :: (Folder, [String]) -> File -> Folder
putFile ((Folder name files folders), []) file = Folder name (file:files) folders
putFile ((Folder name files folders), (parent:children)) folder = do
  let old = pick folders parent
  Folder name files (upsert folders (putFile (old, children) folder))

folderSize :: Folder -> Integer
folderSize (Folder name files []) = fileSize files
folderSize (Folder name files (folder:folders)) = (folderSize folder) + (folderSize (Folder name files folders))

fileSize :: [File] -> Integer 
fileSize ((File _ size):files) = size + (fileSize files)
fileSize [] = 0

allSize :: Folder -> [Integer]
allSize root = do
  let rootSize = folderSize root
  let (Folder _ _ children) = root
  let childrenSize = flatten(map allSize children)
  (rootSize:childrenSize)

flatten :: [[Integer]] -> [Integer]
flatten (i:is) = i ++ (flatten is)
flatten [] = []

partialSum :: [Integer] -> Integer -> Integer
partialSum (i:is) thresh = if i <= thresh 
  then i + (partialSum is thresh)
  else partialSum is thresh
partialSum [] _ = 0