{-# LANGUAGE LambdaCase, TemplateHaskell #-}

import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.Trans.State.Lazy (State)
import Control.Lens

-- Helper functions --
split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
    (_, []) -> [xs]
    (ls, _:rs) -> ls : split sep rs

setElement :: a -> Integer -> [a] -> [a]
setElement x 0 (_:ys) = x:ys
setElement x n (y:ys) = y:setElement x (n - 1) ys

-- Intcode --
data Intcode = Intcode { _mem :: [Integer], _pc :: Int }
makeLenses ''Intcode

executeInstruction :: State Intcode Integer
executeInstruction = readInt >>= \case
    1 -> (+) <$> readIndirect <*> readIndirect >>= writeInt >> executeInstruction
    2 -> (*) <$> readIndirect <*> readIndirect >>= writeInt >> executeInstruction
    99 -> (!! 0) <$> S.gets _mem
    n -> error $ "Unexpected instruction: " ++ show n

readInt :: State Intcode Integer
readInt = (!!) <$> S.gets _mem <*> S.gets _pc <* S.modify (& pc %~ (+1))

readIndirect :: State Intcode Integer
readIndirect = (!!) <$> S.gets _mem <*> (fromInteger <$> readInt)

writeInt :: Integer -> State Intcode ()
writeInt value = readInt >>= \target -> S.modify (& mem %~ setElement value target)

-- Main code --
main :: IO ()
main = do
    numbers <- map read . split ',' . init <$> readFile "../2019/input/day_2.txt"
    print $ part1 numbers (12, 2)
    print $ part2 numbers 19690720

part1 :: [Integer] -> (Integer, Integer) -> Integer
part1 mem (a, b) = fst $ executeInstruction `S.runState` Intcode mem' 0
    where mem' = setElement a 1 (setElement b 2 mem) ++ repeat 0

part2 :: [Integer] -> Integer -> Integer
part2 mem expected = head [100 * a + b | a <- [1..99], b <- [1..99], part1 mem (a, b) == expected]
