-- Kindly stolen from https://github.com/purcell/adventofcode2016
module Day
    ( loadDay
    , runDay
    , Day(..)
    ) where

import Control.Monad      (when)
import System.Environment (getArgs)

data Day i a b = Day
    { dayNum    :: Int
    , dayParser :: String -> i
    , dayPart1  :: i -> a
    , dayPart2  :: i -> b
    }

loadDay :: Day i a b -> IO i
loadDay d = do
    let fp = "../data/day" ++ show (dayNum d) ++ ".txt"
    content <- readFile fp
    return $ dayParser d content

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
