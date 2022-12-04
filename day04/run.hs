import Text.Regex.Posix
import Text.Printf

main :: IO ()
main = do
  input <- readFile "input"
  let assignments = map parse (filter notEmpty (lines input))
  printf "part 1: %d\n" (sum (map contains assignments))
  printf "part 2: %d\n" (sum (map overlap assignments))

-- common
parse :: String -> ((Integer,Integer),(Integer,Integer))
parse line = do
  let r = "([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)"
  let l = (line =~ r :: [[String]])
  affect l

notEmpty :: String -> Bool
notEmpty line = line /= "\n"

affect :: [[String]] -> ((Integer,Integer),(Integer,Integer))
affect [[_,a,b,c,d]] =
  ((read a :: Integer, read b :: Integer), (read c :: Integer, read d :: Integer))
affect _ = ((0,0),(0,0))

-- part 1
contains :: ((Integer,Integer),(Integer,Integer)) -> Integer
contains ((a,b),(c,d)) = if (a>=c && b<=d) || (a<=c && b>=d) then 1 else 0

-- part 2
overlap :: ((Integer,Integer),(Integer,Integer)) -> Integer
overlap ((a,b),(c,d)) = if (b>=c && a<=d) || (d>=a && c<=b) then 1 else 0
