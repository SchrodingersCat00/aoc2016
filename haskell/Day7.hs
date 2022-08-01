module Day7 where

import qualified Data.Set as S
import qualified Data.Set

inputFile :: String
inputFile = "day7.txt"

data IPv7 = IPv7
    { regular   :: [String]
    , hypernet :: [String]
    } deriving (Show)

-- (A, B)
type ABA = (Char, Char)

parseIPv7 :: String -> IPv7
parseIPv7 s = f s [] [] ""
    where
        f "" r h cur = IPv7 { regular = cur:r, hypernet = h}
        f (s:ss) r h cur
            | s == '[' = f ss (cur:r) h ""
            | s == ']' = f ss r (cur:h) ""
            | otherwise = f ss r h (cur++[s])

containsAbba :: String -> Bool
containsAbba (a:b:c:d:xs)
    | a == d && b == c && a /= c = True
    | otherwise = containsAbba (b:c:d:xs)
containsAbba _ = False

unionMap :: (String -> S.Set ABA) -> [String] -> S.Set ABA
unionMap f = foldl applyFAndUnion S.empty
    where
        applyFAndUnion :: S.Set ABA -> String -> S.Set ABA
        applyFAndUnion s x = S.union s (f x)

collectAbas :: String -> S.Set ABA
collectAbas cs@(x:y:z:xs) = foldl addIfAba S.empty $ zip3 cs (y:z:xs) (z:xs)
    where
        addIfAba :: S.Set ABA -> (Char, Char, Char) -> S.Set ABA
        addIfAba s (a, b, c)
            | a == c && a /= b = S.insert (a, b) s
            | otherwise = s
collectAbas _ = S.empty

part :: (IPv7 -> Bool) -> String -> Int
part f = length . filter f . map parseIPv7 . lines

part1 :: String -> Int
part1 = part isValidIPv7
    where
        isValidIPv7 :: IPv7 -> Bool
        isValidIPv7 IPv7 { regular = r, hypernet = h } =
            any containsAbba r && not (any containsAbba h)

part2 :: String -> Int
part2 = part supportsSSL
    where
        supportsSSL :: IPv7 -> Bool
        supportsSSL IPv7 { regular = r, hypernet = h } =
            let
                rabas = unionMap collectAbas r
                habas = unionMap collectAbas h
                in (not . null) $ S.intersection rabas (flipAbas habas)

        flipAbas :: S.Set ABA -> S.Set ABA
        flipAbas = S.map (\(x, y) -> (y, x))

