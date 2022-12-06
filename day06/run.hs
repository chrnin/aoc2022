import Data.List
import Text.Printf

main = do
    input <- readFile "input"
    let bytes = head(lines input)
    let prefix = repeater (head bytes) 4
    printf "part 1: %d\n" (distinctIndex bytes prefix 4 0)
    let prefix = repeater (head bytes) 14
    printf "part 2: %d\n" (distinctIndex bytes prefix 14 0)

-- common
repeater :: Char -> Int -> [Char]
repeater _ 0 = []
repeater a i = a:(repeater a (i-1))

distinctIndex :: String -> [Char] -> Int -> Integer -> Integer
distinctIndex (c:cs) prefix l i = do
    let p = (c:(take (l-1) prefix))
    if (length (nub p)) == (length p)
    then
        i + 1
    else distinctIndex cs p l (i+1)
    