import Data.Maybe (mapMaybe, isJust, fromJust)

main = do
    input <- map (read @Int) . lines <$> readFile "inputs/1.txt"
    print $ fromJust $ findSum input 2020
    print $ findSum' input

findSum :: [Int] -> Int -> Maybe Int
findSum [] _ = Nothing
findSum (x:xs) r
    | search `elem` xs = Just (x * search)
    | otherwise = findSum xs r
    where search = r - x

findSum' :: [Int] -> Int
findSum' (x:xs)
    | isJust result = x * fromJust result
    | otherwise = findSum' xs
    where result = findSum xs (2020 - x)