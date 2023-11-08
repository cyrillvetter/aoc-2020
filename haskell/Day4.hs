import Data.List.Split (splitOn)
import Data.List (isSuffixOf)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import qualified Data.Set as S

hclValidation = S.fromList $ ['0'..'9'] ++ ['a'..'f']
eclValidation = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

main = do
    input <- parse <$> readFile "inputs/4.txt"
    print $ length $ filter (\l -> length l == 7) $ map (map fst) input
    print $ length $ filter id $ map isPassportValid input

parse :: String -> [[(String, String)]]
parse = map (filter (\t -> fst t /= "cid:") . concatMap (map (splitAt 4) . splitOn " ") . lines) . splitOn "\n\n"

isPassportValid :: [(String, String)] -> Bool
isPassportValid p = length valid == 7 && and valid
    where valid = map isValid p

isValid :: (String, String) -> Bool
isValid ("byr:", v) = validateNumber v 1920 2002
isValid ("iyr:", v) = validateNumber v 2010 2020
isValid ("eyr:", v) = validateNumber v 2020 2030
isValid ("hgt:", v)
    | "cm" `isSuffixOf ` v = validateNumber num 150 193
    | "in" `isSuffixOf ` v = validateNumber num 59 76
    | otherwise = False
    where num = takeWhile isDigit v
isValid ("hcl:", v:vs)
    | v == '#' = length vs == 6 && all (`S.member` hclValidation) vs
    | otherwise = False
isValid ("ecl:", v) = v `S.member` eclValidation
isValid ("pid:", v) = length v == 9 && isJust ((readMaybe @Int) v)
isValid _ = False

validateNumber :: String -> Int -> Int -> Bool
validateNumber s lower upper = case m of
    Nothing -> False
    Just r -> r >= lower && r <= upper
    where m = (readMaybe @Int) s