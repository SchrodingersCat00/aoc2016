-- Kindly stolen from https://github.com/purcell/adventofcode2016
module Day
    ( loadDay
    , runDay
    , makeParser
    , Day(..)
    , module P
    ) where

import Text.Parsec as P hiding (State)
import Text.Parsec.String as P
import Control.Monad      (when)
import System.Environment (getArgs)

makeParser :: (String -> a) -> Parser a
makeParser f = many anyChar >>= \s -> return $ f s

data Day i a b = Day
    { dayNum    :: Int
    , dayParser :: Parser i
    , dayPart1  :: i -> a
    , dayPart2  :: i -> b
    }

loadDay :: Day i a b -> IO i
loadDay d = do
    let fp = "../data/day" ++ show (dayNum d) ++ ".txt"
    result <-
        parseFromFile (dayParser d <* eof) fp
    case result of
        Right input -> return input
        Left e -> error (show e)

runDay :: (Show a, Show b) => Day i a b -> IO ()
runDay d = do
    args <- getArgs
    input <- loadDay d
    when (null args || "a" `elem` args) $
      do putStrLn $ banner "a"
         print $ dayPart1 d input
    when (null args || "b" `elem` args) $
      do putStrLn $ "\n" ++ banner "b"
         print $ dayPart2 d input
    where
        banner ab = "==== DAY " ++ show (dayNum d) ++ ab ++ " ====\n"
