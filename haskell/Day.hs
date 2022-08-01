module Day where

class Day a where
    inputFile :: a -> String
    parse :: a -> String -> i
    part1 :: a -> i -> o
    part2 :: a -> i -> o