main = do input <- map (map read . words) . lines <$> readFile "input/day_9.txt"
          let diffs = takeWhile (any (/= 0)) . iterate (\l -> zipWith (-) (tail l) l)
          print $ map (sum . concatMap (map last . diffs)) [input, map reverse input]
