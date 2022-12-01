import Data.List.Split
import Data.List

main :: IO ()
main = do 
  input <- readFile "input"
  let (e1:e2:e3:es) = calories input
  print("part1:")
  print(e1)
  print("part2:")
  print(e1+e2+e3)

calories :: String -> [Integer]
calories input = sortBy (flip compare) (map sum (map toInt (map lines (splitOn "\n\n" input))))

toInt :: [String] -> [Integer]
toInt strings = map read strings :: [Integer]