{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

main = do
  input@[times, distances] <- map (map read . tail . words) . lines <$> readFile "input/day_6.txt"
  print . product $ zipWith solve times distances
  let [time, distance] = read . concatMap (show . round) <$> input
  print $ solve time distance

solve :: Double -> Double -> Int
solve t n = ceiling a - floor b - 1
  where [a, b] = [(t + d * sqrt (t ^ 2 - 4 * (n+1))) / 2 | d <- [1, -1]]
