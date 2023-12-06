{-# LANGUAGE TupleSections #-}

import Data.Char (isDigit)
import Data.List (groupBy, sortBy)
import Data.Function (on)

main :: IO ()
main = do
    input <- parse . lines <$> readFile "input/day_3.txt"
    print $ part1 input
    print $ part2 input

parse :: [String] -> [String]
parse lines = border ++ map (\l -> "." ++ l ++ ".") lines ++ border
    where width = length $ head lines
          border = [replicate (width + 2) '.']

part1 :: [String] -> Int
part1 lines = sum $ concatMap (\(i, line) -> countSymbols i <$> getNumbers line) (zip [0..] lines)
    where countSymbols :: Int -> [(Int, Char)] -> Int
          countSymbols line_number digits =
            let xs = [fst (head digits)-1..fst (last digits)+1]
                ys = [line_number - 1 .. line_number + 1]
            in  if   any isSymbol [(lines !! y) !! x | x <- xs, y <- ys]
                then read $ map snd digits
                else 0

getNumbers :: String -> [[(Int, Char)]]
getNumbers = groupByFn (isDigit . snd) . zip [0..]

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && not (isDigit c)

groupByFn :: (a -> Bool) -> [a] -> [[a]]
groupByFn f = map reverse . go False []
    where go True (a:acc) (c:cs) | f c = go True ((c:a):acc) cs
          go False acc (c:cs) | f c = go True ([c]:acc) cs
          go isGroup acc (_:cs) = go False acc cs
          go isGroup acc [] = acc

part2 :: [String] -> Integer
part2 lines = sum . map (product . map snd) . filter ((== 2) . length) . groupBy ((==) `on` fst) $ sortBy (compare `on` fst) touching_gears
    where touching_gears = concat $ concatMap (\(i, line) -> countSymbols i <$> getNumbers line) (zip [0..] lines)
          countSymbols :: Int -> [(Int, Char)] -> [((Int, Int), Integer)]
          countSymbols line_number digits = map (, read $ map snd digits) gears
            where xs = [fst (head digits)-1..fst (last digits)+1]
                  ys = [line_number - 1 .. line_number + 1]
                  gears = [(x, y) | x <- xs, y <- ys, (lines !! y) !! x == '*']
