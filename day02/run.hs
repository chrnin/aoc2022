import Data.List.Split
import Text.Printf

main = do
  input <- readFile "input"
  let matchInput = readMatch input
  let scorePart1 = map match matchInput 
  printf "part 1: %d\n" (sum scorePart1)
  let scorePart2 = map strategy matchInput
  printf "part 2: %d\n" (sum scorePart2)

readMatch :: String -> [[String]]
readMatch input = map (splitOn " ") (lines input)

convert :: String -> String
convert a 
  | a == "A" = "rock"
  | a == "B" = "paper"
  | a == "C" = "scissors"
  | otherwise = ""

match :: [String] -> Integer
match (a:x:_)
  | x == "X" = (rock (convert a))
  | x == "Y" = (paper (convert a))
  | x == "Z" = (scissors (convert a))
  | otherwise = 0
match _ = 0

strategy :: [String] -> Integer
strategy (a:x:_)
  | x == "X" = (lose (convert a))
  | x == "Y" = (draw (convert a))
  | x == "Z" = (win (convert a))
strategy _ = 0

lose :: String -> Integer
lose val
  | val == "rock" = scissors val
  | val == "paper" = rock val
  | val == "scissors" = paper val
lose _ = 0

draw :: String -> Integer
draw val
  | val == "rock" = rock val
  | val == "paper" = paper val
  | val == "scissors" = scissors val
draw _ = 0

win :: String -> Integer
win val
  | val == "rock" = paper val
  | val == "paper" = scissors val
  | val == "scissors" = rock val
win _ = 0

rock :: String -> Integer
rock val
  | val == "rock" = 1 + 3
  | val == "paper" = 1 + 0
  | val == "scissors" = 1 + 6
  | otherwise = 0

paper :: String -> Integer
paper val
  | val == "rock" = 2 + 6
  | val == "paper" = 2 + 3
  | val == "scissors" = 2 + 0
  | otherwise = 0

scissors :: String -> Integer  
scissors val 
  | val == "rock" = 3 + 0
  | val == "paper" = 3 + 6
  | val == "scissors" = 3 + 3
  | otherwise = 0

