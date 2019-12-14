{-# LANGUAGE LambdaCase #-}

import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.Trans.State.Lazy (State)

main :: IO ()
main = do
    numbers <- map read . split ',' . init <$> readFile "../2019/input/day_2.txt"
    print $ part1 numbers (12, 2)
    print $ part2 numbers 19690720

part1 :: [Integer] -> (Integer, Integer) -> Integer
part1 mem (a, b) = fst $ executeInstruction `S.runState` Intcode mem' 0
    where mem' = setElement 1 a (setElement 2 b mem) ++ repeat 0

part2 :: [Integer] -> Integer -> Integer
part2 mem expected = head [100 * a + b | a <- [1..99], b <- [1..99], part1 mem (a, b) == expected]

-- Intcode --
data Intcode = Intcode { mem :: [Integer], pc :: Int }

executeInstruction :: State Intcode Integer
executeInstruction = readInt >>= \case
    1 -> (+) <$> readIndirect <*> readIndirect >>= writeInt >> executeInstruction
    2 -> (*) <$> readIndirect <*> readIndirect >>= writeInt >> executeInstruction
    99 -> (!! 0) <$> S.gets mem
    n -> error $ "Unexpected instruction: " ++ show n

incrementPc :: State Intcode ()
incrementPc = S.modify $ \program -> program { pc = pc program + 1 }

readInt :: State Intcode Integer
readInt = (!!) <$> S.gets mem <*> S.gets pc <* incrementPc

readIndirect :: State Intcode Integer
readIndirect = (!!) <$> S.gets mem <*> (fromInteger <$> readInt)

writeInt :: Integer -> State Intcode ()
writeInt value = readInt >>= \target ->
    S.modify $ \program -> program { mem = setElement target value (mem program) }

-- Helper functions --
split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
    (_, []) -> [xs]
    (ls, _:rs) -> ls : split sep rs

setElement :: Integer -> a -> [a] -> [a]
setElement 0 x (_:ys) = x:ys
setElement n x (y:ys) = y:setElement (n - 1) x ys
