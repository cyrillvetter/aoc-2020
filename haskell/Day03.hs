main = do
    input <- lines <$> readFile "inputs/3.txt"
    print $ countTrees input 3 1
    print $ part2 input

part2 :: [String] -> Int
part2 input = product $ map (uncurry (countTrees input)) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

countTrees :: [String] -> Int -> Int -> Int
countTrees input right down = length $ filter id $ zipWith isTree [1..] (every down (drop 1 input))
    where
        isTree :: Int -> String -> Bool
        isTree i l = (l !! ((i * right) `mod` length l)) == '#'

every :: Int -> [a] -> [a]
every 2 (x:y:xs) = y : every 2 xs;
every 1 a = a
every _ _ = []