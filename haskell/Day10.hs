import Data.List (sort)

main = do
    input <- sort . map (read @Int) . lines <$>readFile "inputs/10.txt"
    print $ part1 input
    print $ traceAdapters (0 : input)

part1 :: [Int] -> Int
part1 input =
    let differences = zipWith (-) input (0 : input)
    in length (filter (== 1) differences) * (length (filter (== 3) differences) + 1)

traceAdapters :: [Int] -> Integer
traceAdapters [c] = 1
traceAdapters (c:cs) =
    let nextAdapters = zip [0..] $ takeWhile (\n -> (n - c) <= 3) cs
    in sum $ map (\(i, num) -> traceAdapters (drop i cs)) nextAdapters