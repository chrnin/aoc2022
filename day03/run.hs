import Data.Char (ord)
import Text.Printf

main :: IO ()
main = do
  input <- readFile "input"
  let part1 = priorities input
  printf "part 1: %d\n" (sum part1)
  let part2 = map priority (commonItems input)
  printf "part 2: %d\n" (sum part2)

-- common
priority :: Char -> Int
priority item 
  | ord item > 96 = (ord item) - 96
  | ord item > 64 = (ord item) - 38
  | otherwise = 0

-- part1
open :: String -> (String, String)
open rucksack = do
  let l = length rucksack
  splitAt (l `div` 2) rucksack

match :: (String, String) -> Int
match ([], s) = 0
match ((c:cs), s) = do
  if c `elem` s then priority c
  else match (cs, s)

priorities :: String -> [Int]
priorities input = map match (map open (lines input))

-- part2
split3 :: [t] -> [(t,t,t)]
split3 (x1:x2:x3:xs) = ((x1, x2, x3)):(split3 xs) 
split3 _ = []

commonItem :: ([Char],[Char],[Char]) -> Char
commonItem ((i1:is),s2,s3) = do
  if ((i1 `elem` s2) && (i1 `elem` s3)) then 
    i1
  else 
    commonItem (is,s2,s3)
commonItem _ = '$'

commonItems :: String -> [Char]
commonItems input = map commonItem (split3 (lines input))