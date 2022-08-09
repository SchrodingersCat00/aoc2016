module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day12
import qualified Day16
import qualified Day19


main :: IO ()
main = do
    input <- readFile ("../data/" ++ Day19.inputFile)
    putStrLn $ "Part1: " ++ show (Day19.part1 input)
    putStrLn $ "Part2: " ++ show (Day19.part2 input)