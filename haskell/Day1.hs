module Main where

import Control.Monad (forM_, unless)
import Data.Either (fromRight)
import qualified Data.Set as S

import Day

data Rotation = TurnLeft | TurnRight deriving (Show)

type Instruction = (Rotation, Int)

type Direction = (Int, Int)
type Position = (Int, Int)

type Pose = (Position, Direction)

readInstruction :: String -> Instruction
readInstruction ('L':xs) = (TurnLeft, read xs)
readInstruction ('R':xs) = (TurnRight, read xs)
readInstruction _        = error "Unexpected character"

parseInstructions :: String -> [Instruction]
parseInstructions i = map readInstruction $ (words . filter (/=',')) i

rotate :: Direction -> Rotation -> Direction
rotate (x, y) TurnLeft  = (-y, x)
rotate (x, y) TurnRight = (y, -x)

step :: Pose -> Instruction -> Pose
step ((x, y), d) (r, s) = let d'@(dx, dy) = rotate d r in ((x+dx*s, y+dy*s), d')

initPose :: Pose
initPose = ((0, 0), (0, 1))

runInstructions :: [Instruction] -> Position
runInstructions i = fst $ foldl step initPose i

steps :: Pose -> [Instruction] -> [Position]
steps p []     = [fst p]
steps p (i:is) = let next = step p i in fst next:steps next is

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

distance :: Position -> Int
distance p = uncurry (+) $ mapPair abs p

-- part1 :: String -> Int
part1 :: [Instruction] -> Int
part1 = distance . runInstructions

-- part2 :: String -> Int
part2 :: [Instruction] -> Int
part2 = distance . go
    where
        go :: [Instruction] -> Position
        go is = case revisitedPosition (intermediatePositions (steps initPose is)) of
            Just x  -> x
            Nothing -> error "No solution was found"

range :: Int -> Int -> [Int]
range x y
    | x < y = init [x..y]
    | x > y = init [x, x-1..y]
    | otherwise = [x]

-- fills in the missing positions [(1, 2), (1, 4)] -> [(1, 2), (1, 3), (1, 4)]
intermediatePositions :: [Position] -> [Position]
intermediatePositions [] = []
intermediatePositions [x] = [x]
intermediatePositions (x:y:xs) = positionsBetween x y ++ intermediatePositions (y:xs)
    where
        positionsBetween :: Position -> Position -> [Position]
        positionsBetween (x, y) (x', y') = do
            xs <- range x x'
            ys <- range y y'
            return (xs, ys)

revisitedPosition :: [Position] -> Maybe Position
revisitedPosition = f S.empty
    where
        f _ [] = Nothing
        f s (p:ps)
            | S.member p s = Just p
            | otherwise    = f (S.insert p s) ps

main :: IO ()
main =
    runDay $
    Day
        1
        parseInstructions
        part1
        part2
