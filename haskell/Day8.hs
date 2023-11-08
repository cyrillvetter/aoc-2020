import qualified Data.IntSet as IS
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

main = do
    input <- parse <$> readFile "inputs/8.txt"
    print $ findLoopAcc 0 input IS.empty 0
    print $ part2 input

findLoopAcc :: Int -> [(String, Int)] -> IS.IntSet -> Int -> Int
findLoopAcc i instructions visited acc
    | IS.member i visited = acc
    | otherwise = case instructions !! i of
        ("acc", n) -> findLoopAcc (i + 1) instructions (IS.insert i visited) (acc + n)
        ("jmp", n) -> findLoopAcc (i + n) instructions (IS.insert i visited) acc
        ("nop", _) -> findLoopAcc (i + 1) instructions (IS.insert i visited) acc

part2 :: [(String, Int)] -> Int
part2 input =
    let nopJmpIndices = map fst $ filter (\(_, (instr, _)) -> instr == "nop" || instr == "jmp") $ zip [0..] input
    in head $ mapMaybe (\i -> getFiniteAcc 0 i input IS.empty 0) nopJmpIndices

getFiniteAcc :: Int -> Int -> [(String, Int)] -> IS.IntSet -> Int -> Maybe Int
getFiniteAcc i switchIndex instructions visited acc
    | IS.member i visited = Nothing
    | otherwise = if i >= length instructions || i < 0
        then Just acc
        else case nextInstruction of
        ("acc", n) -> getFiniteAcc (i + 1) switchIndex instructions (IS.insert i visited) (acc + n)
        ("jmp", n) -> getFiniteAcc (i + n) switchIndex instructions (IS.insert i visited) acc
        ("nop", _) -> getFiniteAcc (i + 1) switchIndex instructions (IS.insert i visited) acc
        where next = instructions !! i
              nextInstruction = if i == switchIndex then switchInstruction next else next

switchInstruction :: (String, Int) -> (String, Int)
switchInstruction (instr, i) = if instr == "jmp" then ("nop", i) else ("jmp", i)

parse :: String -> [(String, Int)]
parse = map ((\[l, r] -> (l, readNum r)) . splitOn " ") . lines

readNum :: String -> Int
readNum (x:xs)
    | x == '+' = read xs
    | otherwise = negate $ read xs