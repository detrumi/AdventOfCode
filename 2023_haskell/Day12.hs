{-# LANGUAGE LambdaCase #-}

import qualified Data.Map as M
import Data.Map (Map)
import qualified Control.Monad.State.Lazy as S
import Control.Monad.State.Lazy (State)
import Data.List (intercalate)

main = do
    (springs, arrangements) <- unzip . map (parse . words) . lines <$> readFile "input/day_12.txt"
    print . sum $ zipWith part1 springs arrangements
    let (springs', arrangements') = (map (intercalate "?" . replicate 5) springs, map (concat . replicate 5) arrangements)
    print . sum $ zipWith (\s a -> S.evalState (part2 s a) M.empty) springs' arrangements'

parse :: [String] -> (String, [Int])
parse [springs, groups] = (springs, map read $ split ',' groups)

part1 :: String -> [Int] -> Int
part1 ss [] = if '#' `notElem` ss then 1 else 0
part1 [] _ = 0
part1 ('?':ss) gs = part1 ss gs + part1 ('#':ss) gs
part1 ('.':ss) gs = part1 ss gs
part1 ss (g:gs) = if isGroup then part1 (drop (g+1) ss) gs else 0
    where isGroup = length ss >= g && '.' `notElem` take g ss && (length ss == g || ss !! g /= '#')


part2 :: String -> [Int] -> State (Map (String, [Int]) Int) Int
part2 ss gs = S.gets (M.lookup (ss, gs)) >>= \case
    Nothing -> part2' ss gs >>= \n -> S.modify (M.insert (ss,gs) n) >> return n
    Just n -> return n

part2' :: String -> [Int] -> State (Map (String, [Int]) Int) Int
part2' ss [] = return $ if '#' `notElem` ss then 1 else 0
part2' [] _ = return 0
part2' ('?':ss) gs = (+) <$> part2 ss gs <*> part2 ('#':ss) gs
part2' ('.':ss) gs = part2 ss gs
part2' ss (g:gs) = if isGroup then part2 (drop (g+1) ss) gs else return 0
    where isGroup = length ss >= g && '.' `notElem` take g ss && (length ss == g || ss !! g /= '#')


split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
    (_, []) -> [xs]
    (ls, _:rs) -> ls : split sep rs
