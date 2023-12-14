import Data.List (transpose, sortBy)
import Data.Ord (Down(Down), comparing)
import qualified Data.Map.Lazy as M

main = do
    input <- transpose . lines <$> readFile "input/day_14.txt"
    print . sum $ concatMap (load . tilt) input
    print . sum . concatMap load $ findLoop M.empty input

findLoop m ss = case M.lookup ss m of
    Just n -> iterate spin ss !! ((1000000000 - n) `mod` (M.size m - n))
    Nothing -> M.insert ss (M.size m) m `findLoop` spin ss

spin = (!! 4) . iterate (rotate . map tilt)
rotate ss = map (\n -> map (!! (length ss - n)) ss) [1..length ss]
load l = map ((length l -) . fst) . filter ((== 'O') . snd) $ zip [0..] l

tilt [] = []
tilt s@('.':_) = sortBy (comparing Down) a ++ tilt b
    where (a,b) = span (/= '#') s
tilt (c:cs) = c : tilt cs
