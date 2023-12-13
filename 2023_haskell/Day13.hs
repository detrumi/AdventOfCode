import Data.List (transpose)

main = do
    input <- split [] . lines <$> readFile "input/day_13.txt"
    print . sum $ map reflection input
    print . sum $ map (\ls -> head $ filter (\r -> r > 0 && r /= reflection ls) $ map (reflection' (reflection ls)) $ smudgeAll ls) input

reflection :: [String] -> Int
reflection ls = case map go [ls, transpose ls] of
    [n:_, _] -> 100 * n
    [_, n:_] -> n
    _ -> 0
    where go ls = filter (reflect ls) [1..length ls - 1]

reflection' :: Int -> [String] -> Int
reflection' x ls = case map (filter (/= x)) [map (*100) (go ls), go (transpose ls)] of
    [n:_, _] -> n
    [_, n:_] -> n
    _ -> 0
    where go ls = filter (reflect ls) [1..length ls - 1]



reflect :: [String] -> Int -> Bool
reflect ls n = and $ zipWith (==) (reverse a) b
    where (a,b) = splitAt n ls


smudgeAll :: [String] -> [[String]]
smudgeAll [] = []
smudgeAll (l:ls) = map (:ls) (smudgeLine l) ++ map (l:) (smudgeAll ls)

smudgeLine :: String -> [String]
smudgeLine l = map (go . flip splitAt l) [0..length l-1]
    where go (a,[]) = a
          go (a,b:bs) = a ++ [smudge b] ++ bs

smudge '#' = '.'
smudge '.' = '#'

split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
    (_, []) -> [xs]
    (ls, _:rs) -> ls : split sep rs
