import Data.List (group, sort, transpose)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Control.Monad as M

main :: IO ()
main = do
    codes <- lines <$> readFile "input/day_2.txt"
    print $ part1 codes
    print $ part2 codes

part1 :: [String] -> Int
part1 = product . map (length . filter id) . transpose . map go
    where go :: String -> [Bool]
          go = zipWith elem [2, 3] . repeat . map length . group . sort

part2 :: [String] -> String
part2 = go []
    where go :: [String] -> [String] -> String
          go prev (c:cs) = case catMaybes $ map (match c) prev of
            s:_ -> s
            _ -> go (c:prev) cs

match :: String -> String -> Maybe String
match xs ys = case filter (not . snd) ([0..] `zip` zipWith (==) xs ys) of
    [(i, False)] -> Just (as ++ bs)
        where (as, b:bs) = splitAt i xs
    _ -> Nothing
