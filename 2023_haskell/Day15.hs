import Data.Char (ord)

main = do
    input <- split ',' . concat . lines <$> readFile "input/day_15.txt"
    print . sum $ map hash input
    print $ part2 input

hash = foldl ((((`mod` 256) . (*17)) .) . (+)) 0 . map ord

part2 = sum . zipWith (*) [1..] . map (sum . zipWith (*) [1..] . map (read . snd)) . initialize (replicate 256 [])

initialize bs [] = bs
initialize bs (op:ops)
    | '-' `elem` op = initialize (updateBox (filter ((/= init op) . fst)) (hash $ init op) bs) ops
    | otherwise = let
    [l,n] = split '=' op
    go b = if   any ((== l) . fst) b
           then map (\(l',n') -> if l' == l then (l,n) else (l',n')) b
           else b ++ [(l,n)]
    in initialize (updateBox go (hash l) bs) ops

updateBox f n boxes = as ++ [f b] ++ bs
    where (as,b:bs) = splitAt n boxes

split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
    (_, []) -> [xs]
    (ls, _:rs) -> ls : split sep rs
