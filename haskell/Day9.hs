module Main where

import Data.Char (isDigit)

import Day 

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

main :: IO ()
main = 
    runDay $
    Day
        9
        id
        part1
        part2