module Day19 where

import Control.Monad (liftM2, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (STUArray (STUArray))
import qualified Data.Array.MArray as A
import Data.Array.ST (newListArray, readArray, runSTArray,
                                    writeArray)
import qualified Data.Sequence     as S
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import Day

inputFile :: String
inputFile = "day19.txt"

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            when x $ f >> go

party' :: Int -> Int
party' n = (`S.index` 0) . f $ S.iterateN n (+1) 1
    where
        f s
            | S.length s >= 2 = f (S.drop 2 s S.>< S.take 1 s)
        f s = s

party :: Int -> Int
party n = runST $ do
    i <- newSTRef 0
    a <- newListArray (0, n) ([1..n-1]++[0]) :: ST s ((STUArray s) Int Int)
    whileM_ (readSTRef i >>= \iv -> readArray a iv >>= \el -> return (iv /= el)) $ do
        iv <- readSTRef i
        el <- readArray a iv
        nb <- readArray a el
        writeArray a iv nb
        writeSTRef i el
        return ()
    readSTRef i

part1 _ = party' 3018458

part2 _ = 0

main :: IO ()
main =
    runDay $
    Day 
        19
        (read :: String -> Int)
        part1
        part2