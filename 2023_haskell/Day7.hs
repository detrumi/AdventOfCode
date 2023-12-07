import Data.List (group, elemIndex, sort, sortBy, transpose)
import Data.Function (on)
import Data.Ord (comparing, Down (Down))

main = do [hands, bids] <- transpose . map words . lines <$> readFile "input/day_7.txt"
          print . solve "AKQJT98765432" id id $ zip hands bids
          print . solve "AKQT98765432J" (filter (/= 'J')) ((\(_:ls) -> 5-sum ls:ls) . (++ [0])) $ zip hands bids

solve s f g = let handType = g . sortBy (comparing Down) . map length . group . sort . f
              in  sum . zipWith (*) [1..] . map (read . snd) . sortBy ((\a b -> mconcat $ comparing handType a b : zipWith (comparing (`elemIndex` s)) b a) `on` fst)

