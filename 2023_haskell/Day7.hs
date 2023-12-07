import Data.List (group, sort, sortBy, elemIndex, sortBy, transpose)
import Data.Function (on)
import Data.Maybe (fromJust)

main = do
    [hands, bids] <- transpose . map words . lines <$> readFile "input/day_7.txt"
    print $ part1 $ zip hands bids
    print $ part2 $ zip hands bids

part1 = sum . zipWith (*) [1..] . map (read . snd) . sortBy ((\a b -> (compare `on` handType) b a <> secondOrdering b a) `on` fst)
secondOrdering a b = head . filter (/= EQ) $ zipWith compareStrength a b
handType = fromJust . (`elemIndex` [[5],[1,4],[2,3],[1,1,3],[1,2,2],[1,1,1,2],[1,1,1,1,1]]) . sort . map length . group . sortBy compareStrength
compareStrength = compare `on` flip elemIndex "AKQJT98765432"

part2 = sum . zipWith (*) [1..] . map (read . snd) . sortBy ((\a b -> (compare `on` handType') b a <> secondOrdering' b a) `on` fst)
secondOrdering' a b = head . filter (/= EQ) $ zipWith compareStrength' a b
handType' hand = fromJust . (`elemIndex` [[5],[1,4],[2,3],[1,1,3],[1,2,2],[1,1,1,2],[1,1,1,1,1]]) . (\l -> if not (null l) then init l ++ [last l + n] else [5])
    . sort . map length . group . sortBy compareStrength' $ filter (/= 'J') hand
    where n = length $ filter (== 'J') hand
compareStrength' = compare `on` flip elemIndex "AKQT98765432J"
