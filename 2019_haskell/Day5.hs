{-# LANGUAGE LambdaCase, TemplateHaskell #-}

import Control.Lens
import qualified Control.Monad.RWS as RWS
import Control.Monad.RWS (RWS)
import Data.List (genericIndex)
import Control.Monad (when)

-- Helper functions --
split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
    (_, []) -> [xs]
    (ls, _:rs) -> ls : split sep rs

setElement :: a -> Integer -> [a] -> [a]
setElement x 0 (_:ys) = x:ys
setElement x n (y:ys) = y:setElement x (n - 1) ys

-- Intcode --
data Intcode = Intcode {
    _mem :: [Integer],
    _pc :: Integer,
    _params :: [Integer]
    }
makeLenses ''Intcode

makeIntcode :: [Integer] -> Intcode
makeIntcode mem = Intcode mem 0 []

type Program = RWS Integer [Integer] Intcode

executeInstruction :: Program ()
executeInstruction = do
    n <- readInt
    RWS.modify (& params .~ [
        n `div` 100 `mod` 10,
        n `div` 1000 `mod` 10,
        n `div` 10000 `mod` 10])
    let n' = n `mod` 100
    when (n' /= 99) (executeInstruction' n' >> executeInstruction)

executeInstruction' :: Integer -> Program ()
executeInstruction' = \case
    1 -> binaryOp (+) >>= writeInt
    2 -> binaryOp (*) >>= writeInt
    3 -> RWS.ask >>= writeInt
    4 -> readIndirect >>= \n -> RWS.tell [n]
    5 -> readIndirect >>= \n -> if n /= 0 then readSp else RWS.modify (& pc %~ succ)
    6 -> readIndirect >>= \n -> if n == 0 then readSp else RWS.modify (& pc %~ succ)
    7 -> binaryOp (<) >>= writeInt . fromIntegral . fromEnum
    8 -> binaryOp (==) >>= writeInt . fromIntegral . fromEnum
    n -> error $ "Unexpected instruction: " ++ show n
    where readSp = readIndirect >>= \n -> RWS.modify (& pc .~ n)
          binaryOp f = f <$> readIndirect <*> readIndirect

readInt :: Program Integer
readInt = genericIndex <$> RWS.gets _mem <*> RWS.gets _pc <* RWS.modify (& pc %~ succ)

readIndirect :: Program Integer
readIndirect = do
    param <- head <$> RWS.gets _params
    RWS.modify (& params %~ tail)
    case param of
        0 -> genericIndex <$> RWS.gets _mem <*> readInt
        1 -> readInt

writeInt :: Integer -> Program ()
writeInt value = readInt >>= \target -> RWS.modify (& mem %~ setElement value target)

-- Main code --
main :: IO ()
main = do
    numbers <- map read . split ',' . init <$> readFile "../2019/input/day_5.txt"
    print $ calculate numbers 1
    print $ calculate numbers 5

calculate :: [Integer] -> Integer -> Integer
calculate mem input = last . snd $
    RWS.execRWS executeInstruction input (makeIntcode $ mem ++ repeat 0)
