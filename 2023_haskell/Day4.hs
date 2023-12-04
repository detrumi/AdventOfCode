import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "input/day_4.txt"
    print $ part1 input
    print $ part2 input

parse :: String -> ([Int], [Int])
parse = both (map read) . fmap (drop 1) . break (== "|") . drop 2 . words

part1 = sum . map (getScore . countWinners)

countWinners = length . uncurry Set.intersection . both Set.fromList

getScore 0 = 0
getScore n = 2 ^ (n - 1)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

part2 lines = go (length lines `replicate` 1) lines
    where
    go (c:cs) (l:ls) = c + go ((+c) `map` take score cs ++ drop score cs) ls
        where score = countWinners l
    go [] [] = 0
