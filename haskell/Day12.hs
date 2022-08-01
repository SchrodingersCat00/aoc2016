module Day12 where

import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Array.IArray as IA
import Text.Read (readMaybe)
import Debug.Trace

import Utils
inputFile :: String
inputFile = "day12.txt"

type Reg = Char
type Registry = M.Map Reg Int

data Operand = Register Reg | Constant Int deriving (Show)

getValue :: Registry -> Operand -> Int
getValue _ (Constant x) = x
getValue reg (Register r) = M.findWithDefault 0 r reg

data Instruction = Copy Operand Reg
                 | Inc Reg
                 | Dec Reg
                 | Jnz Operand Int
                 deriving (Show)

parseOperand :: String -> Operand
parseOperand x = case readMaybe x :: Maybe Int of
    Just x' -> Constant x'
    Nothing -> Register $ head x

parseInstruction :: String -> Instruction
parseInstruction = f . words
    where
        f :: [String] -> Instruction
        f ["cpy", x, y] = Copy (parseOperand x) (head y)
        f ["inc", a] = Inc $ head a
        f ["dec", a] = Dec $ head a
        f ["jnz", x, y] = Jnz (parseOperand x) (read y)
        f _ = error "This should not happen"

type Program = A.Array Int Instruction

data ProgramState = State
    { program :: Program
    , pc :: Int
    , registry :: Registry
    , finished :: Bool
    } deriving Show

emptyState1 :: Program -> ProgramState
emptyState1 p = State { program = p, pc = 0, registry = M.empty, finished = False }

emptyState2 :: Program -> ProgramState
emptyState2 p = State { program = p, pc = 0, registry = M.fromList [('c', 1)], finished = False }

getInstruction :: Program -> Int -> Maybe Instruction
getInstruction p i = let
    (l, u) = IA.bounds p
    in
        if l <= i  && i <= u
        then Just $ p IA.! i
        else Nothing

runNextInstruction :: ProgramState -> ProgramState
runNextInstruction s@State { program = p, pc = pc, registry = reg } =
    case getInstruction p pc of
        Nothing -> s { finished = True}
        Just i -> case i of
            Copy x r -> s { pc = pc+1, registry = M.insert r (getValue reg x) reg }
            Inc r -> s { pc = pc+1, registry = M.insertWith (+) r 1 reg }
            Dec r -> s { pc = pc+1, registry = M.insertWith (+) r (-1) reg }
            Jnz n i -> if getValue reg n /= 0
                then s { pc = pc+i }
                else s { pc = pc+1 }

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> a
iterateWhile c f = head . dropWhile c . iterate f

part :: (Program -> ProgramState) -> String -> Int
part e x = let
    is = (map parseInstruction . lines) x
    program = IA.listArray (0, length is - 1) is
    initState = e program
    states = iterate runNextInstruction initState
    finalState = iterateWhile (not . finished) runNextInstruction initState
    in registry finalState M.! 'a'

part1 :: String -> Int
part1 = part emptyState1

part2 :: String -> Int
part2 = part emptyState2