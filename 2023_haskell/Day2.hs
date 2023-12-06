{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

type Bag = [Int]

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "input/day_2.txt"
    print $ part1 input
    print $ part2 input


parse :: String -> [Bag]
parse = map parseBag . split ';' . drop 2 . dropWhile (/= ':')

parseBag :: String -> Bag
parseBag = foldl1 (zipWith max) . map (makeBag . words) . split ','

makeBag :: [String] -> Bag
makeBag [n, "red"] = [read n, 0, 0]
makeBag [n, "green"] = [0, read n, 0]
makeBag [n, "blue"] = [0, 0, read n]


part1 :: [[Bag]] -> Int
part1 = sum . map fst . filter (all (and . zipWith (>=) [12, 13, 14]) . snd) . zip [1..]

part2 :: [[Bag]] -> Int
part2 = sum . map (product . foldl1 (zipWith max))


split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
    (_, []) -> [xs]
    (ls, _:rs) -> ls : split sep rs
