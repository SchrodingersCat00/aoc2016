{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Day2 where

import Prelude hiding (Left, Right)
import Control.Monad.State (evalState, get, put, State)
import Control.Monad ( forM )
import Utils

inputFile = "day2.txt"

data Direction = Up | Right | Down | Left

-- (horizontal, vertical)
type PadPosition = (Int, Int)
type Keypad = PadPosition -> Maybe Char

squareKeypad :: PadPosition -> Maybe Char
squareKeypad (1, -1) = Just '1'
squareKeypad (1, 0) = Just '2'
squareKeypad (1, 1) = Just '3'
squareKeypad (0, -1) = Just '4'
squareKeypad (0, 0) = Just '5'
squareKeypad (0, 1) = Just '6'
squareKeypad (-1, -1) = Just '7'
squareKeypad (-1, 0) = Just '8'
squareKeypad (-1, 1) = Just '9'
squareKeypad _ = Nothing

diamondKeypad :: PadPosition -> Maybe Char
diamondKeypad (2, 0) = Just '1'
diamondKeypad (1, -1) = Just '2'
diamondKeypad (1, 0) = Just '3'
diamondKeypad (1, 1) = Just '4'
diamondKeypad (0, -2) = Just '5'
diamondKeypad (0, -1) = Just '6'
diamondKeypad (0, 0) = Just '7'
diamondKeypad (0, 1) = Just '8'
diamondKeypad (0, 2) = Just '9'
diamondKeypad (-1, -1) = Just 'A'
diamondKeypad (-1, 0) = Just 'B'
diamondKeypad (-1, 1) = Just 'C'
diamondKeypad (-2, 0) = Just 'D'
diamondKeypad _ = Nothing

snapToPad :: PadPosition -> PadPosition
snapToPad (x, y) = (signum x, signum y)

parseDirections :: String -> [[Direction]]
parseDirections s = map parseLine $ lines s
    where
        parseLine :: String -> [Direction]
        parseLine = map $ \c ->
            case c of
                'U' -> Up
                'R' -> Right
                'D' -> Down
                'L' -> Left
                _ -> error "Unexpected character encountered"

findCode :: PadPosition -> Keypad -> [[Direction]] -> String
findCode ip kp ds = flip evalState ip $ forM ds (findDigit kp)

findDigit :: Keypad -> [Direction] -> State PadPosition Char
findDigit kp ds = do
    pos <- get
    let endPos = foldl (takeStep kp) pos ds
    put endPos
    case kp endPos of
        Just p -> return p
        Nothing -> error "This should not happen"

takeStep :: Keypad -> PadPosition -> Direction -> PadPosition
takeStep kp p@(x, y) Up = choosePosition kp p (x+1, y)
takeStep kp p@(x, y) Right = choosePosition kp p (x, y+1)
takeStep kp p@(x, y) Down = choosePosition kp p (x-1, y)
takeStep kp p@(x, y) Left = choosePosition kp p (x, y-1)

choosePosition :: Keypad -> PadPosition -> PadPosition -> PadPosition
choosePosition kp p p' = case kp p' of
    Just _ -> p'
    Nothing -> p

part1 :: String -> String
part1 = findCode (0, 0) squareKeypad . parseDirections

part2 :: String -> String
part2 = findCode (0, -2) diamondKeypad . parseDirections