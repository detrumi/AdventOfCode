import Data.List (transpose)

main = do
    input <- lines <$> readFile "input/day_11.txt"
    let galaxies :: [(Int, Int)] = concat $ zipWith (\y -> map ((y,) . fst) . filter ((== '#') . snd) . zip [0..]) [0..] input
        [ys, xs] = map fst . filter (all (== '.') . snd) . zip [0..] <$> [input, transpose input]
        expand a b = length . filter (\n -> n > min a b && n < max a b)
        solve n = sum [abs (y2-y1) + abs (x2-x1) + n * (expand y1 y2 ys + expand x1 x2 xs) | a@(y1,x1) <- galaxies, b@(y2,x2) <- galaxies, a < b]
    print $ map solve [1, 1000000 - 1]
