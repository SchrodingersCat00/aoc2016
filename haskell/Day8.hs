module Main where

import Control.Monad (forM, forM_, unless)
import qualified Control.Monad.ST as ST
import qualified Data.Array.ST as S
import qualified Data.Array.Unboxed as U
import Data.Char (isDigit)

import Day

data Instruction = Rect Int Int
                 | ShiftRow Int Int
                 | ShiftCol Int Int
                 deriving (Show)

data Screen = Screen
    { pixels :: U.UArray (Int, Int) Bool
    , width  :: Int
    , height :: Int
    }

instance Show Screen where
    show Screen { pixels = p, width = w, height = h } =
        '\n' : unlines [unwords [disp (p U.! (r, c)) | c <- [0..w-1]] | r <- [0..h-1]]
        where
            disp :: Bool -> String
            disp True  = "X"
            disp False = "."

emptyScreen :: Screen
emptyScreen = let
    w = 50
    h = 6
    pixels = U.listArray ((0, 0), (h-1, w-1)) (replicate (h*w) False)
    in Screen pixels w h

splitOn :: (Eq a) => a -> [a] -> ([a], [a])
splitOn c xs = splitOn' c [] xs
    where
        splitOn' _ prev [] = (prev, [])
        splitOn' c prev (x:xs)
            | c == x = (prev, xs)
            | otherwise = splitOn' c (prev ++ [x]) xs

parseInstruction :: String -> Instruction
parseInstruction = f . words
    where
        f :: [String] -> Instruction
        f ["rect", v] = let (x, y) = splitOn 'x' v in Rect (read x) (read y)
        f ["rotate", "column", v, "by", xs] = let (_, y) = splitOn '=' v in ShiftCol (read y) (read xs)
        f ["rotate", "row", v, "by", xs] = let (_, y) = splitOn '=' v in ShiftRow (read y) (read xs)
        f _ = error "Unexpected input"

rotateMod :: Int -> Int -> Int
rotateMod x y = let res = x `mod` y in
    if res < 0
        then y + res
        else res

run :: Screen -> [Instruction] -> Screen
run s@Screen { pixels = p, width = w, height = h } is = let
    newPixels = S.runSTUArray $ do
        pixels <- S.thaw p
        forM_ is $ \i -> do
            case i of
                Rect x y -> forM_ [(j, i) | i <- [0..x-1], j <- [0..y-1]] $ \idx ->
                    S.writeArray pixels idx True
                ShiftCol idx by -> do
                    col <- forM [0..h-1] (\r -> S.readArray pixels (r, idx))
                    forM_ (zip col ([by..h-1] ++ [0..by-1])) $ \(nv, r) -> do
                        S.writeArray pixels (r, idx) nv
                ShiftRow idx by -> do
                    row <- forM [0..w-1] (\c -> S.readArray pixels (idx, c))
                    forM_ (zip row ([by..w-1] ++ [0..by-1])) $ \(nv, c) -> do
                        let ni = (c - by) `rotateMod` h
                        S.writeArray pixels (idx, c) nv
        return pixels
    in s { pixels = newPixels }

countOnPixels :: Screen -> Int
countOnPixels = foldl (\c b -> if b then c+1 else c) 0 . U.elems . pixels

part1 :: [Instruction] -> Int
part1 = countOnPixels . run emptyScreen

part2 :: [Instruction] -> Screen
part2 = run emptyScreen

main :: IO ()
main =
    runDay $
    Day
        8
        (map parseInstruction . lines)
        part1
        part2