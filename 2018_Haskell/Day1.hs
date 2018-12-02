import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
    numbers <- map (read . dropWhile (== '+')) . lines <$> readFile "input/day_1.txt"
    print $ part1 numbers
    print $ part2 numbers

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = go S.empty . scanl1 (+) . cycle
    where go :: Set Int -> [Int] -> Int
          go found (s:ss)
            | S.member s found = s
            | otherwise = go (S.insert s found) ss
