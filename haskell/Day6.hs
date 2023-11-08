import Data.List (intersect)
import Data.List.Split (splitOn)
import qualified Data.Set as S

main = do
    input <- map lines . splitOn "\n\n" <$> readFile "inputs/6.txt"
    print $ sum $ map (length . S.fromList . concat) input
    print $ sum $ map (length . intersectAll) input

intersectAll :: Eq a => [[a]] -> [a]
intersectAll = foldr1 intersect