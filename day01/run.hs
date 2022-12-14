import Data.List.Split
import Data.List
import Text.Printf

main :: IO ()
main = do 
  input <- readFile "input"
  let (e1:e2:e3:es) = calories input
  printf "part 1: %d\n" e1
  printf "part 2: %d\n" (e1+e2+e3)

calories :: String -> [Integer]
calories input = sortBy (flip compare) (map sum (map toInt (map lines (splitOn "\n\n" input))))

toInt :: [String] -> [Integer]
toInt strings = map read strings :: [Integer]