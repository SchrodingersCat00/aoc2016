module Day3 where

import Data.Text (replace)

import Day

type Triangle = (Int, Int, Int)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

countTrue :: [Bool] -> Int
countTrue = foldl (\n b -> if b then n+1 else n) 0

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = (a + b) > c && (a + c) > b && (b + c) > a

parseTriangle :: String -> Triangle
parseTriangle s = t $ (map read . words) s
    where
        t [x, y, z] = (x, y, z)
        t z         = error $ "Invalid line" ++ show z

part1 :: String -> Int
part1 x = countTrue $ uncurry3 isTriangle . parseTriangle <$> lines x

part2 :: String -> Int
part2 x = countTrue $ map (uncurry3 isTriangle) $ parse $ concatMap words $ lines x
    where
        parse :: [String] -> [Triangle]
        parse [] = []
        parse (x1:x2:x3:y1:y2:y3:z1:z2:z3:xs) = parseT x1 y1 z1:parseT x2 y2 z2:parseT x3 y3 z3:parse xs
        parse _ = error "This is impossible"

        parseT :: String -> String -> String -> (Int, Int, Int)
        parseT x y z = (read x, read y, read z)

main :: IO ()
main = 
    runDay $
    Day
        3
        id
        part1
        part2