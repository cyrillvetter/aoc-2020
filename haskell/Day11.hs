import Data.List (transpose)

main = do
    input <- lines <$> readFile "inputs/11.txt"
    print $ sum $ map (length . filter (== '#')) $ part1 input

part1 :: [[Char]] -> [[Char]]
part1 seats =
    let surrounded = surround seats
        yWindows = windowsOf 3 surrounded
        xWindows = map (transpose . map (windowsOf 3)) yWindows
        next = map (map newSeat) xWindows
    in if seats == next then seats else part1 next

newSeat :: [[Char]] -> Char
newSeat [x, [a, seat, b], y]
    | seat == 'L' = if occupied == 0 then '#' else 'L'
    | seat == '#' = if occupied >= 4 then 'L' else '#'
    | otherwise = seat
    where
        occupied =
            let ca = if a == '#' then 1 else 0
                cb = if b == '#' then 1 else 0
                cx = length $ filter (== '#') x
                cy = length $ filter (== '#') y
            in ca + cb + cx + cy

surround :: [[Char]] -> [[Char]]
surround seats =
    let core = map (\inner -> '.' : inner ++ ['.']) seats
        outer = replicate (length $ head core) '.'
    in outer : core ++ [outer]

windowsOf :: Int -> [a] -> [[a]]
windowsOf size list
    | length list < size = []
    | otherwise = take size list : windowsOf size (drop 1 list)