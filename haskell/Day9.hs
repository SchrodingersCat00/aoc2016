module Day9 where

import Data.Char(isDigit)
import Utils (eztrace)

inputFile :: String
inputFile = "day9.txt"

decompress :: String -> String
decompress x = decompress' x ""
    where
        decompress' :: String -> String -> String
        decompress' [] s = s
        decompress' ('(':xs) s = let
            len = (read . takeWhile isDigit) xs
            amount = (read . takeWhile (/=')') . tail . dropWhile (/= 'x')) xs
            rest = (tail . dropWhile (/=')')) xs
            interesting = take len rest
            remainder = drop len rest
            in decompress' remainder (s ++ (concat . replicate amount) interesting)
        decompress' (x:xs) s = decompress' xs (s ++ [x])

part1 :: String -> Int
part1 = length . decompress . filter (/=' ')

part2 x = ""