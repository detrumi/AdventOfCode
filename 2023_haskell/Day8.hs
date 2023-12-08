import Data.Char (isAlphaNum)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main = do
    (instructions:_:maze) <- lines <$> readFile "input/day_8.txt"
    let maze' = Map.fromList . map ((\(h:t) -> (h,t)) . chunksOf 3 . filter isAlphaNum) $ maze
    print $ part1 (cycle instructions) maze' "AAA"
    print . foldr1 lcm $ map (getLoops (cycle $ zip [0..] instructions) maze' []) . filter ((== 'A') . (!! 2)) $ Map.keys maze'

part1 :: String -> Map String [String] -> String -> Int
part1 _ _ "ZZZ" = 0
part1 (i:is) maze node = 1 + part1 is maze ((maze ! node) !! turn i)

getLoops ((ix,i):is) maze visited node = case elemIndex (ix,node) visited of
    Just prev -> prev + 1
    Nothing   -> getLoops is maze ((ix,node):visited) $ (maze ! node) !! turn i

turn = fromJust . (`elemIndex` "LR")


chunksOf n [] = []
chunksOf n l = let (a,b) = splitAt n l in a:chunksOf n b
