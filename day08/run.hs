import Text.Printf
main :: IO ()

main = do
    input <- readFile "input"
    let grid = parseInput (lines input)
    let (x,y) = dim grid
    let v = map (visible grid) (every grid)
    printf "part 1: %d\n" (count v)
    let s = map (sceneScore grid) (every grid)
    printf "part 2: %d\n" (maximum s)

parseInput :: [String] -> [[Integer]]
parseInput (l:ls) = (parseLine l):(parseInput ls)
parseInput [] = []

parseLine :: String -> [Integer]
parseLine (c:cs) = ((read [c] :: Integer):parseLine cs) 
parseLine [] = []

get :: [[Integer]] -> (Int, Int) -> Integer
get ((c:cs):ls) (0,0) = c
get ((c:cs):ls) (x,0) = get (cs:ls) (x-1,0)
get (l:ls) (x,y) = get ls (x,(y-1))
get [] _ = 0

getXSlice :: [[Integer]] -> ((Int,Int), Int) -> [Integer]
getXSlice grid ((xmin, xmax), y) = if xmin < xmax 
  then
    ((get grid (xmin,y)):(getXSlice grid ((xmin+1,xmax), y)))
  else
    []

getYSlice :: [[Integer]] -> (Int, (Int, Int)) -> [Integer]
getYSlice grid (x, (ymin, ymax)) = if ymin < ymax 
  then
    ((get grid (x,ymin)):(getYSlice grid (x, (ymin+1, ymax))))
  else
    []


dim :: [[Integer]] -> (Int, Int) 
dim grid = (length (head grid), length grid)

every :: [[Integer]] -> [(Int, Int)] 
every grid = [(x,y) | y <- [0..((length grid)-1)], x <- [0..((length (head grid))-1)]]

visible :: [[Integer]] -> (Int, Int) -> Bool
visible grid (x,y) = do
  let (xmax, ymax) = dim grid
  let val = get grid (x,y)
  let left = gt (getXSlice grid ((0,x), y)) val
  let right = gt (getXSlice grid ((x+1,xmax), y)) val
  let top = gt (getYSlice grid (x,(0, y))) val
  let bottom = gt (getYSlice grid (x,(y+1, ymax))) val
  left || right || top || bottom
  
gt :: [Integer] -> Integer -> Bool
gt (l:ls) i = if i > l then gt ls i else False
gt [] i = True

count :: [Bool] -> Integer
count [] = 0
count (b:bs) = if b 
  then 1 + count bs 
  else count bs

sceneScore :: [[Integer]] -> (Int, Int) -> Integer
sceneScore grid origin = ((sceneScoreDirection grid origin origin (-1,0))* -- left
  (sceneScoreDirection grid origin origin (1,0))*                          -- right
  (sceneScoreDirection grid origin origin (0,-1))*                         -- to 
  (sceneScoreDirection grid origin origin (0,1)))                          -- bottom

sceneScoreDirection :: [[Integer]] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Integer
sceneScoreDirection grid origin (x,y) (xdir, ydir) = do
  let (xmax, ymax) = dim grid
  if (x == xmax-1 || y == ymax-1 || x == 0 || y == 0)
    then 0
    else if (get grid ((x+xdir),(y+ydir))) >= (get grid origin)
      then 1
      else 1 + (sceneScoreDirection grid origin (x+xdir,y+ydir) (xdir, ydir))
  

