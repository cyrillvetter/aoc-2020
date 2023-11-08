main = do
    input <- map (read @Integer) . lines <$> readFile "inputs/9.txt"
    let invalidNumber = findInvalidNumber input
    print invalidNumber
    print $ findContiguous input invalidNumber

findInvalidNumber :: [Integer] -> Integer
findInvalidNumber input =
    let preample = take 25 input
        num = input !! 25
    in if checkPreamble preample num then findInvalidNumber $ drop 1 input else num
    where
        checkPreamble :: [Integer] -> Integer -> Bool
        checkPreamble [x] _ = False
        checkPreamble (x:xs) n = (n - x) `elem` xs || checkPreamble xs n

findContiguous :: [Integer] -> Integer -> Integer
findContiguous input invalid =
    let contiguous = takeWhile (<= invalid) $ scanl1 (+) input
        nums = take (length contiguous) input
    in if last contiguous == invalid then minimum nums + maximum nums else findContiguous (drop 1 input) invalid