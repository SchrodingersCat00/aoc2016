module Day8 where
import Data.Char (isDigit)
import qualified Data.Array.ST as S
import qualified Data.Array.Unboxed as U
import Control.Monad (forM_)

inputFile :: String
inputFile = "day8.txt"

data Instruction = Rect Int Int
                 | ShiftRow Int Int
                 | ShiftCol Int Int
                 deriving (Show)

data Screen = Screen 
    { pixels :: U.UArray (Int, Int) Bool
    , width :: Int
    , height :: Int
    } deriving (Show)

emptyScreen :: Screen
emptyScreen = let
    w = 7
    h = 3
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
                ShiftCol idx by -> forM_ [0..h] $ \r -> do
                    v <- S.readArray pixels ((r - by) `rotateMod` h, idx)
                    S.writeArray pixels (r, idx) v
                _ -> return ()
        return pixels
    in s { pixels = newPixels }


part1 = run emptyScreen . map parseInstruction . lines

part2 x = ""