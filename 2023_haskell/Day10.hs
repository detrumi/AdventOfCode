import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust, catMaybes)
import qualified Data.Set as S
import Data.Set (Set)

main = do
    input <- parse . lines <$> readFile "input/day_10.txt"
    print $ part1 input
    print $ part2 input

parse :: [String] -> [String]
parse lines = border ++ map (\l -> "." ++ l ++ ".") lines ++ border
    where width = length $ head lines
          border = [replicate (width + 2) '.']

part1 input = let
    start = findStart input
    lookup (y,x) = (input !! y) !! x
    go n [(_,p1), (_,p2)] | p1 == p2 = n
    go n ps = let
        target (prev, from) = (from,) . head . filter (\to -> to /= prev && connects from to (lookup to) && connects to from (lookup from)) $ neighbors from
        in go (n + 1) $ map target ps
    in 1 + (go 0 . map (start,) . filter (\to -> connects start to (lookup to)) $ neighbors start)

diff (y1,x1) (y2,x2) = (y2-y1,x2-x1)

connects from to c = c `elem` case diff from to of
    ( 0, 1) -> "-J7"
    ( 0,-1) -> "-LF"
    ( 1, 0) -> "|LJ"
    (-1, 0) -> "|7F"

neighbors (x,y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx == 0) /= (dy == 0)]

findStart = fmap fromJust . head . filter (isJust . snd) . zip [0..] . map (elemIndex 'S')

part2 input = let
    start = findStart input
    go input' n ps@[(_,p1), (_,p2)] | p1 == p2 = ps ++ n
    go input' n ps = let
        target (prev, from) = (from,) . head . filter (\to -> to /= prev && connects from to (lookup' input' to) && connects to from (lookup' input' from)) $ neighbors from
        in go input' (ps ++ n) $ map target ps
    connections = S.fromList $ go input [] . map (start,) . filter (\to -> connects start to (lookup' input to)) $ neighbors start

    expanded = expand input connections 0
    start' = findStart expanded
    connections' = go expanded [] . map (start',) . filter (\to -> connects start' to (lookup' expanded to)) $ neighbors start'
    pipes' = S.fromList $ start' : map snd connections'
    dots' = [(y,x) | y <- [0..length expanded-1], x <- [0..length (head expanded)-1], not (isBorder expanded (y,x)), lookup' expanded (y,x) /= ' ', S.notMember (y,x) pipes']
    part2b :: [Pos] -> Set Pos -> [Maybe [Pos]]
    part2b (p:ps) filled = results : part2b ps pipes' --filled'
        where (results, filled') = fill expanded filled [p]
    part2b [] _ = []
    in length . S.fromList . concat . catMaybes $ part2b dots' pipes'

type Pos = (Int, Int)

expand input connections y | y == length input = []
expand input connections y = concat (zipWith right [0..] (input !! y)) : concat (zipWith down [0..] (input !! y)) : expand input connections (y + 1)
    where right x c | ((y,x), (y,x+1)) `S.member` connections || ((y,x+1), (y,x)) `S.member` connections = [c, '-']
                    | otherwise = [c, ' ']
          down x c | ((y,x), (y+1,x)) `S.member` connections || ((y+1,x), (y,x)) `S.member` connections = ['|', ' ']
                   | otherwise = [' ', ' ']

isBorder input (y,x) = y == 0 || x == 0 || y == length input - 1 || x == length (head input) - 1

lookup' input (y,x) = (input !! y) !! x

fill1 :: [String] -> Set Pos -> [Pos] -> (Maybe [Pos], Set Pos)
fill1 input filled current = let
    ns = S.fromList $ concatMap (filter (`S.notMember` filled) . neighbors) current
    dots = filter ((/= ' ') . lookup' input) (S.toList ns)
    filled' = S.union filled ns
    in if null ns then (Just dots, filled') else if any (isBorder input) ns then (Nothing, filled') else (\(a,b) -> if isJust a then ((dots ++) <$> a, b) else (Nothing, b)) $ fill input filled' (S.toList ns)

fill :: [String] -> Set Pos -> [Pos] -> (Maybe [Pos], Set Pos)
fill input filled current = let
    ns = S.fromList . filter (`S.notMember` filled) $ current
    dots = filter ((/= ' ') . lookup' input) (S.toList ns)
    filled' = S.union filled ns
    in if null ns then (Just dots, filled') else if any (isBorder input) ns then (Nothing, filled') else (\(a,b) -> if isJust a then ((dots ++) <$> a, b) else (Nothing, b)) $ fill input filled' (concatMap neighbors $ S.toList ns)
