module Day16 where

import Utils

inputFile :: String
inputFile = "day16.txt"

bitFlip :: String -> String
bitFlip = map bitFlipC
    where
        bitFlipC '1' = '0'
        bitFlipC '0' = '1'
        bitFlipC c = error $ "Unexpected bit character: " ++ [c]

checkSum :: String -> String
checkSum = head . dropWhile (even . length) . iterate checkSumStep
    where
        checkSumStep [] = []
        checkSumStep [_] = error "Cannot calculate checksum for single character"
        checkSumStep (x:y:xs) 
            | x == y = '1':checkSumStep xs
            | otherwise = '0':checkSumStep xs

explode :: String -> String
explode s = s ++ ['0'] ++ (reverse . bitFlip) s

part :: Int -> String -> String
part ds = checkSum
        . take ds
        . until ((>=ds) . length) explode

part1 :: String -> String
part1 = part 272

part2 :: String -> String
part2 = part 35651584