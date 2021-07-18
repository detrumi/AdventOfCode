import qualified Data.Set as S
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- Helper functions --
split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
    (_, []) -> [xs]
    (ls, _:rs) -> ls : split sep rs

-- Data types --
type Pos = (Int, Int)

data Direction = U | R | D | L
    deriving (Show, Read)

goDir :: Direction -> Pos -> Pos
goDir U (x, y) = (x, y + 1)
goDir R (x, y) = (x + 1, y)
goDir D (x, y) = (x, y - 1)
goDir L (x, y) = (x - 1, y)

data Step = Step Direction Int
    deriving Show

parseStep :: String -> Step
parseStep (d:cs) = Step (read [d]) (read cs)

-- Main code --
main :: IO ()
main = do
    wires <- map (map parseStep . split ',') . lines <$> readFile "../2019/input/day_3.txt"
    print $ part1 wires
    print $ part2 wires

part1 :: [[Step]] -> Int
part1 = minimum . map manhattanDistance . intersections . map positions

part2 :: [[Step]] -> Int
part2 wires = minimum $ map combinedSteps (intersections ps)
    where ps = map positions wires
          combinedSteps pos = sum $ map (succ . fromJust . elemIndex pos) ps

manhattanDistance :: Pos -> Int
manhattanDistance (x, y) = abs x + abs y

intersections :: [[Pos]] -> [Pos]
intersections = S.toList . foldl1 S.intersection . map S.fromList

positions :: [Step] -> [Pos]
positions = go (0,0)
    where go :: Pos -> [Step] -> [Pos]
          go _ [] = []
          go pos (Step _ 0:ss) = go pos ss
          go pos (Step d n:ss) = pos' : go pos' (Step d (pred n):ss)
              where pos' = goDir d pos
