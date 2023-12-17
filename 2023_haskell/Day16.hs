{-# LANGUAGE LambdaCase #-}
import Data.Array
import qualified Control.Monad.State.Lazy as S
import Control.Monad.State.Lazy (State)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Array.Base (IArray(numElements))

data Dir = North | East | South | West deriving (Enum, Eq, Show)
type Pos = (Int, Int)

main = do
    input <- parse . lines <$> readFile "input/day_16.txt"
    let solve start = length $ S.execState (energize input start) Set.empty
    print $ solve ((-1,0), East)
    print . maximum . map solve $ borders input

parse ls = listArray (0, length ls - 1) $ map (\l -> listArray (0, length l - 1) l) ls

energize :: Array Int (Array Int Char) -> (Pos, Dir) -> State (Set Pos) ()
energize m (from, dir) = let pos = move from dir in case tile m pos of
       Nothing -> return ()
       Just t -> S.gets (Set.member pos) >>= \case
           True | t == '-' || t == '|' -> return ()
           _ -> do S.modify (Set.insert pos)
                   mapM_ (energize m . (pos,)) (nextDirs t dir)

nextDirs '/' dir = (:[]) $ case dir of
    North -> East
    East -> North
    South -> West
    West -> South
nextDirs '\\' dir = (:[]) $ case dir of
    North -> West
    West -> North
    South -> East
    East -> South
nextDirs '|' dir | dir == East || dir == West = [North, South]
nextDirs '-' dir | dir == North || dir == South = [East, West]
nextDirs _ dir = [dir]

move (x,y) dir = case dir of North -> (x,y-1)
                             East -> (x+1,y)
                             South -> (x,y+1)
                             West -> (x-1,y)

tile m (x,y) | inRange (bounds m) y && inRange (bounds $ m ! y) x = Just $ m ! y ! x
             | otherwise = Nothing

borders m = concat $ [[((x,height),North), ((x,-1), South)] | x <- [0..height-1]]
    ++ [[((-1,y),East), ((width,y),West)] | y <- [0..width-1]]
    where height = numElements m
          width = numElements (m ! 0)
