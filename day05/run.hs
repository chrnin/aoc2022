import Data.List
import Data.Char
import Text.Printf

main :: IO ()

main = do
  input <- readFile "input"
  let inputLines = lines input
  let crates = readCrates inputLines
  let moves = readMoves inputLines
  let items = readItems inputLines (length crates)
  let populatedCrates = populateCrates crates (reverse items)
  let part1 = applyMoves9000 moves populatedCrates
  printf "part 1: %s\n" (readMessage part1)
  let part2 = applyMoves9001 moves populatedCrates
  printf "part 2: %s\n" (readMessage part2)
  

readCrates :: [String] -> [[Char]]
readCrates [] = []
readCrates (l:ls) = do
  if (isPrefixOf " 1" l) 
    then 
      readCrate l
    else
      readCrates ls

readCrate :: String -> [[Char]]
readCrate (c:cs)
  | c == ' ' = readCrate cs
  | otherwise = ([]:(readCrate cs))
readCrate [] = []

readMoves :: [String] -> [(Int, Int, Int)]
readMoves (l:ls) = do
  if (isPrefixOf "move" l)
  then 
    (readMove l):(readMoves ls)
  else 
    (readMoves ls)
readMoves [] = []

readMove :: String -> (Int, Int, Int)
readMove line 
  | line == "" = (0,0,0)
  | otherwise = do
      let stack = takeInt (drop 5 line)
      let from = takeInt (drop (11 + (length stack)) line)
      let to = takeInt (drop (15 + ((length stack) + (length from))) line)
      (read stack :: Int,read from :: Int,read to :: Int)

takeInt :: String -> String
takeInt (s:xs) 
  | (isDigit s) = s:(takeInt xs)
  | otherwise = ""
takeInt [] = ""

readItems :: [String] -> Int -> [[String]]

readItems [] _ = []

readItems (l:ls) cratesLength = do
  if (isPrefixOf "[" l || isPrefixOf " " l) && not (isPrefixOf " 1" l)
  then
    ((takeItems l cratesLength):(readItems ls cratesLength))
  else
    readItems ls cratesLength
  
takeItems :: String -> Int -> [String]
takeItems _ 0 = []

takeItems "" i = (" ":(takeItems "" (i-1)))

takeItems line i = do
  if ((length line) >= 2) 
  then
    ((take 1 (drop 1 line)):(takeItems (drop 4 line) (i-1)))
  else
    (" ":(takeItems "" (i-1)))

appendItems :: [[Char]] -> [String] -> [[Char]]
appendItems (crate:crates) (item:items) = do
  if item /= " " then 
    ((head item):crate):(appendItems crates items)
  else crate:(appendItems crates items)
appendItems _ _ = []

populateCrates :: [[Char]] -> [[String]] -> [[Char]]
populateCrates crates (items:allItems) = do
  let newCrates = appendItems crates items
  populateCrates newCrates allItems
populateCrates crates [] = crates

applyMove9001 :: [[Char]] -> [[Char]] -> (Int, Int, Int) -> Int -> [[Char]]
applyMove9001 (crate:crates) allCrates (stack,from,to) i 
  | i == from = do 
    (drop stack crate):(applyMove9001 crates allCrates (stack,from,to) (i+1))
  | i == to = do
    let f = head (drop (from-1) allCrates)
    ((take stack f) ++ crate):(applyMove9001 crates allCrates (stack,from,to) (i+1))
  | otherwise = crate:(applyMove9001 crates allCrates (stack,from,to) (i+1))
applyMove9001 [] _ _ _ = []

applyMoves9001 :: [(Int,Int,Int)] -> [[Char]] -> [[Char]]
applyMoves9001 (move:moves) crates = do
  let newCrates = applyMove9001 crates crates move 1
  applyMoves9001 moves newCrates
applyMoves9001 [] crates = crates

applyMove9000 :: [[Char]] -> [[Char]] -> (Int, Int, Int) -> Int -> [[Char]]
applyMove9000 (crate:crates) allCrates (stack,from,to) i 
  | i == from = do 
    (drop stack crate):(applyMove9000 crates allCrates (stack,from,to) (i+1))
  | i == to = do
    let f = head (drop (from-1) allCrates)
    ((reverse (take stack f)) ++ crate):(applyMove9000 crates allCrates (stack,from,to) (i+1))
  | otherwise = crate:(applyMove9000 crates allCrates (stack,from,to) (i+1))
applyMove9000 [] _ _ _ = []

applyMoves9000 :: [(Int,Int,Int)] -> [[Char]] -> [[Char]]
applyMoves9000 (move:moves) crates = do
  let newCrates = applyMove9000 crates crates move 1
  applyMoves9000 moves newCrates
applyMoves9000 [] crates = crates

readMessage :: [[Char]] -> String
readMessage (x:xs) =   (head x):readMessage(xs)
readMessage [] = ""