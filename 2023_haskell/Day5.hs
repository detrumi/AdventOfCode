{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Maybe (catMaybes)

main = do
    s:m <- split "" . lines <$> readFile "input/day_5.txt"
    let seeds :: [Int] = map read . tail . words $ head s
    let maps :: [[[Int]]] = map (map read . words) . tail <$> m
    print . minimum $ map (`part1` maps) seeds
    print . minimum . map fst $ flip part2 maps . map (\[a,b] -> (a, a + b - 1)) $ chunksOf 2 seeds

part1 :: Int -> [[[Int]]] -> Int
part1 = foldl $ \n m -> head . catMaybes $ map (convert n) m ++ [Just n]

convert :: Int -> [Int] -> Maybe Int
convert n [dest, src, len] | n >= src && n < src + len = Just $ n - src + dest
                           | otherwise = Nothing


part2 :: [(Int, Int)] -> [[[Int]]]  -> [(Int, Int)]
part2 = foldl $ \ranges m -> concatMap (go m) ranges

go :: [[Int]] -> (Int, Int) -> [(Int, Int)]
go ([dest, src, len]:ms) (l,r) = sides ++ middle
  where range a b = let (a', b') = (l `max` a, b `min` r) in [(a', b') | a' <= b']
        sides = concatMap (go ms) $ range l (src-1) ++ range (src+len) r
        middle = both (+(dest-src)) <$> range src (src+len-1)
go [] r = [r]


split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
    (_, []) -> [xs]
    (ls, _:rs) -> ls : split sep rs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = g:chunksOf n gs
    where (g, gs) = splitAt n xs

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)
