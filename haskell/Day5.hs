import Data.List (sort)

main = do
    seatIds <- map (calculateSeatId . splitAt 7) . lines <$> readFile "inputs/5.txt"
    print $ maximum seatIds
    print $ findEmptySeat $ sort seatIds

calculateSeatId :: (String, String) -> Int
calculateSeatId (col, row) = reduceRange col 0 127 * 8 + reduceRange row 0 7

reduceRange :: String -> Int -> Int -> Int
reduceRange [n] lower upper
    | n == 'L' || n == 'F' = lower
    | otherwise = upper
reduceRange (n:ns) lower upper
    | n == 'L' || n == 'F' = reduceRange ns lower (upper - half)
    | otherwise = reduceRange ns (lower + half) upper
    where half = ceiling (fromIntegral (upper - lower) / 2)

findEmptySeat :: [Int] -> Int
findEmptySeat (x:xs)
    | head xs - x > 1 = x + 1
    | otherwise = findEmptySeat xs