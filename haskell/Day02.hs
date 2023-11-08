import Data.List.Split (splitOn)
import Data.Bits (xor)

main = do
    parsed <- map parseLine . lines <$> readFile "inputs/2.txt"
    print $ length $ filter passwordValid parsed
    print $ length $ filter passwordValid' parsed

parseLine :: String -> (Int, Int, Char, String)
parseLine l =
    let split = splitOn " " l
        firstPart = splitOn "-" (head split)
        lower = read $ head firstPart
        upper = read $ firstPart !! 1
        char = head (split !! 1)
        password = split !! 2
    in (lower, upper, char, password)

passwordValid :: (Int, Int, Char, String) -> Bool
passwordValid (lower, upper, char, password) =
    let len = length $ filter (char ==) password
    in len >= lower && len <= upper

passwordValid' :: (Int, Int, Char, String) -> Bool
passwordValid' (i1, i2, char, password) = (password !! (i1 - 1) == char) `xor` (password !! (i2 - 1) == char)