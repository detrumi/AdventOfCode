import Data.Char (isDigit)
import Data.List
import Data.List.Extra (replace)

main :: IO ()
main = do
    input <- lines <$> readFile "input/day_1.txt"
    print $ part1 input
    print $ part2 input

part1 :: [String] -> Int
part1 = sum . map (read . (\s -> [head s, last s]) . filter isDigit)

part2 :: [String] -> Int
part2 = part1 . map replaceNumbers

replaceNumbers :: String -> String
replaceNumbers = foldr ((.)  . replaceNumber) id [1..9]

replaceNumber :: Int -> String -> String
replaceNumber n = replace spelled (spelled ++ show n ++ spelled)
    where spelled = numbers !! n

numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
