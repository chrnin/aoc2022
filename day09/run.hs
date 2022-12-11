import Data.List
import Text.Printf

main :: IO ()

main = do
  input <- readFile "input"
  let motions = readMotions (lines input)
  let headPath = path (steps motions) (0,0)
  let tailPath1 = followMotions headPath (0,0)
  let tailPath2 = followMotions tailPath1 (0,0)
  let tailPath3 = followMotions tailPath2 (0,0)
  let tailPath4 = followMotions tailPath3 (0,0)
  let tailPath5 = followMotions tailPath4 (0,0)
  let tailPath6 = followMotions tailPath5 (0,0)
  let tailPath7 = followMotions tailPath6 (0,0)
  let tailPath8 = followMotions tailPath7 (0,0)
  let tailPath9 = followMotions tailPath8 (0,0)
  printf "part 1: %d\n" (numUniques tailPath1)
  printf "part 2: %d\n" (numUniques tailPath9)

readMotions :: [String] -> [(Char, Int)]
readMotions (l:ls) = ((head l, read (tail (tail l)) ::Int):(readMotions ls))
readMotions [] = []

steps :: [(Char, Int)] -> [(Int, Int)]
steps [] = []
steps ((_, 0):motions) = steps motions
steps ((dir,len):motions) 
  | dir == 'R' = ((1,0):(steps ((dir, (len-1)):motions)))
  | dir == 'L' = ((-1,0):(steps ((dir, (len-1)):motions)))
  | dir == 'U' = ((0,1):(steps ((dir, (len-1)):motions)))
  | dir == 'D' = ((0,-1):(steps ((dir, (len-1)):motions)))
  | otherwise = [(-10,-10)]

path :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
path ((x,y):ss) (startX, startY) = ((startX, startY):(path ss (startX+x, startY+y)))
path [] (startX, startY) = [(startX, startY)]

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (hx, hy) (tx, ty) = do
  if ((abs (tx - hx)) <=1 && (abs (ty - hy)) <=1) 
    then (tx,ty)
    else moveTail (hx,hy) (tx,ty)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hx, hy) (tx, ty) 
  | hx == tx = (tx, ty + ((hy-ty) `div` (abs (hy-ty))))
  | hy == ty = (tx + ((hx-tx) `div` (abs (hx-tx))), ty)
  | otherwise = (tx + ((hx-tx) `div` (abs (hx-tx))), ty + ((hy-ty) `div` (abs (hy-ty))))

followMotions :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
followMotions (h:hs) t = follow h t:(followMotions hs (follow h t))
followMotions [] t = [t]

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub