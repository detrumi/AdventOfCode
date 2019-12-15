import Data.List (group)

main :: IO ()
main = do
    let bounds = map show [382345..843167]
    print $ part1 bounds
    print $ part2 bounds

part1 :: [String] -> Int
part1 = length . filter (any (>= 2) . map length . group) . filter isIncreasing

part2 :: [String] -> Int
part2 = length . filter (any (== 2) . map length . group) . filter isIncreasing

isIncreasing :: String -> Bool
isIncreasing s = and $ zipWith (>=) (tail s) s
