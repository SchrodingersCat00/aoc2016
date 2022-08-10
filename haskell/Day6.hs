module Main where

import Data.List (maximumBy)
import qualified Data.Map  as M

import Day

type Counts = [M.Map Char Int]

getCounts :: [String] -> Counts
getCounts = foldl getCountsForLine (repeat M.empty)
    where
        getCountsForLine :: Counts -> String -> Counts
        getCountsForLine = zipWith increment

        increment :: M.Map Char Int -> Char -> M.Map Char Int
        increment m c = M.insertWith (+) c 1 m

part :: ((Char, Int) -> (Char, Int) -> Ordering) -> String -> [Char]
part f = map ((fst . maximumBy f) . M.toList) . getCounts . lines

part1 :: String -> [Char]
part1 = part compareCount
    where
        compareCount x y = compare (snd x) (snd y)

part2 :: String -> [Char]
part2 = part compareMin
    where
        compareMin x y = compare (snd y) (snd x)

main :: IO ()
main = 
    runDay $
    Day
        6
        id
        part1
        part2
