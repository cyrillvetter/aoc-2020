import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as M

main = do
    input <- map (splitOn " bags contain " . init) . lines <$> readFile "inputs/7.txt"
    let bags = buildMap input M.empty

    -- Subtract one because "shiny gold" bags are also included.
    print $ length (filter (`containsShinyGold` bags) $ M.keys bags) - 1
    print $ countBags "shiny gold" bags - 1

buildMap :: [[String]] -> M.Map String [(Int, String)] -> M.Map String [(Int, String)]
buildMap [[x, y]] map = M.insert x (buildInner y) map
buildMap ([x, y]:s) map = buildMap s (M.insert x (buildInner y) map)

buildInner :: String -> [(Int, String)]
buildInner "no other bags" = []
buildInner l = map bld $ splitOn ", " l
    where
        bld :: String -> (Int, String)
        bld s = (read h, unwords $ init hs)
            where (h:hs) = words s

containsShinyGold :: String -> M.Map String [(Int, String)] -> Bool
containsShinyGold "shiny gold" _ = True
containsShinyGold n map = any (\(_, n) -> containsShinyGold n map) $ fromJust (M.lookup n map)

countBags :: String -> M.Map String [(Int, String)] -> Int
countBags n bags = case innerBags of
    [] -> 1
    _ -> sum (map (\(t, n) -> t * countBags n bags) innerBags) + 1
    where innerBags = fromJust (M.lookup n bags)