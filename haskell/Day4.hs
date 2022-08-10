module Main where

import Data.Char (chr, isAlpha, isNumber, ord)
import Data.List (sortBy, isSubsequenceOf)
import qualified Data.Map as M
import qualified Data.Set as S

import Day

type Occurence = (Char, Int)

data Room = Room
    { roomName :: String
    , sectorID :: Int
    , checksum :: String
    } deriving (Show)

parseRoom :: String -> Room
parseRoom s =
    let
        stripped = filter (/='-') s
        roomName = takeWhile isAlpha stripped
        lastPart = dropWhile isAlpha stripped
        sectorID = (takeWhile isNumber . dropWhile isAlpha) stripped
        checksum = (takeWhile (/=']') . tail . dropWhile (/='[')) stripped
    in Room { roomName = roomName, sectorID = read sectorID, checksum = checksum}


topFive :: String -> [Char]
topFive s = map fst $ take 5 $ sortBy occurenceCompare $ M.toList $ foldl incrementChar M.empty s
    where
        incrementChar :: M.Map Char Int -> Char -> M.Map Char Int
        incrementChar m c = M.insertWith (+) c 1 m

        occurenceCompare :: Occurence -> Occurence -> Ordering
        occurenceCompare o o' = let res = compare (snd o') (snd o) in
            case res of
            EQ -> compare (fst o) (fst o')
            _  -> res

isReal :: Room -> Bool
isReal r = S.fromList (topFive (roomName r)) == S.fromList (checksum r)

rotate :: String -> Int -> String
rotate s n = map (rotateChar n) s
    where
        rotateChar :: Int -> Char -> Char
        rotateChar n c = chr (((ord c - ord 'a' + n) `mod` 26) + ord 'a')

decryptRoom :: Room -> Room
decryptRoom Room { roomName = r, sectorID = s, checksum = c } =
    Room { roomName = rotate r s, sectorID = s, checksum = c}

part1 :: [Room] -> Int
part1 = foldl (\n r -> if isReal r then n+sectorID r else n) 0

part2 :: [Room] -> Int
part2 = sectorID 
      . head 
      . filter hasNorthPole 
      . map decryptRoom 
      . filter isReal
    where
        hasNorthPole Room { roomName = rn } =
            "northpole" `isSubsequenceOf` rn

main :: IO ()
main =
    runDay $
    Day
        4
        (map parseRoom . lines)
        part1
        part2
