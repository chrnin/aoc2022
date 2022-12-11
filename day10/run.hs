import Text.Printf
main :: IO ()

main = do
  input <- readFile "input"
  let signals = exec (lines input) 1
  printf "part1: %d\n" (strength signals)
  let picture = lighten signals 1
  putStr (printPicture picture 0)
  
strength :: [Integer] -> Integer
strength signals = ((head (take 1 (drop 19 signals)))*20)+
  ((head (take 1 (drop 59 signals)))*60)+
  ((head (take 1 (drop 99 signals)))*100)+
  ((head (take 1 (drop 139 signals)))*140)+
  ((head (take 1 (drop 179 signals)))*180)+
  ((head (take 1 (drop 219 signals)))*220)

exec :: [String] -> Integer -> [Integer]
exec (l:ls) signal 
  | l == "noop" = (signal:(exec ls signal))
  | otherwise = (signal:signal:(exec ls (addx l signal)))
exec [] _ = []

addx :: String -> Integer -> Integer
addx s n = n + (read (drop 5 s) :: Integer)

lighten :: [Integer] -> Integer -> [Bool] 
lighten (s:ss) position = ((position >= s && position  <= (s+2)):(lighten ss ((((position) `mod` 40)) +1)))
lighten [] _ = []

printPicture :: [Bool] -> Integer -> String
printPicture (p:ps) col = do
  if col == 40
    then '\n':printPicture (p:ps) 0
    else if p 
      then '#':printPicture (ps) (col+1)      
      else ' ':printPicture (ps) (col+1)      
printPicture [] _ = []
