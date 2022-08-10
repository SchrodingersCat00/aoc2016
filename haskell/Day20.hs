module Day20 where

import Data.List (sortBy)

import Day

type Range = (Integer, Integer)

parseRange :: String -> Range
parseRange = readTuple . break (=='-')
    where
        readTuple (x, y) = (read x, (read . tail) y)

firstNonConnected :: [Range] -> Integer
firstNonConnected [] = error "Empty list"
firstNonConnected [(_, x)] = x
firstNonConnected ((x, x'):(y, y'):xs)
    | y > x'+1 = x'+1
    | y' <= x' = firstNonConnected ((x, x'):xs)
    | y' > x' = firstNonConnected ((y, y'):xs)
    | otherwise = error "We expected a sorted list!"

countGaps :: Integer -> [Range] -> Integer
countGaps n [] = n
countGaps n [(_, x)] = n
countGaps n ((x, x'):(y, y'):xs)
    | y > x'+1 = countGaps (n - 1 + (y - x')) ((x,x'):xs)
    | y' <= x' = countGaps n ((x, x'):xs)
    | y' > x' = countGaps n ((y, y'):xs)
    | otherwise = error "We expected a sorted list!"

part1 :: [Range] -> Integer
part1 = firstNonConnected
      . (++[(4294967296, 4294967296)])
      . sortBy (\x y -> compare (fst x) (fst y))

part2 :: [Range] -> Integer
part2 = countGaps 0
      . ((-1, -1):)
      . (++[(4294967296, 4294967296)])
      . sortBy (\x y -> compare (fst x) (fst y))

main :: IO ()
main = 
    runDay $
    Day
        20
        (map parseRange . lines)
        part1
        part2