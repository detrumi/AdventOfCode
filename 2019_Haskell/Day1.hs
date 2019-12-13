main :: IO ()
main = do
    numbers <- map read . lines <$> readFile "input/day_1.txt"
    print $ part1 numbers
    print $ part2 numbers

part1 :: [Int] -> Int
part1 = sum . map fuel

fuel :: Int -> Int
fuel 0 = 0
fuel n = n `div` 3 - 2

part2 :: [Int] -> Int
part2 = sum . map fuel'

fuel' :: Int -> Int
fuel' n | n > 0 = n' + fuel' n'
    where n' = fuel n
fuel' _ = 0
