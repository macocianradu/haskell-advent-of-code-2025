module Main (main) where

import System.Environment

main :: IO ()
main = do
    path <- getArgs
    file <- readFile $ path !! 0
    let instructions = map  parseInput $ lines file
    let result = foldl rotate (0, 50) instructions 
    print result

parseInput :: String -> (Rotation, Int)
parseInput (dir:num) = (getRotation dir, read num)
parseInput _ = error "Invalid Input"

data Rotation = LEFT | RIGHT deriving (Show)

getRotation :: Char -> Rotation
getRotation 'L' = LEFT
getRotation 'R' = RIGHT
getRotation _ = error "Invalid Rotation"


rotate :: (Int, Int) -> (Rotation, Int) -> (Int, Int)
rotate (acc, pos) (RIGHT, num) = ((pos + num) `div` 100 + acc, (pos + num) `mod` 100)
rotate (acc, pos) (LEFT, num) = let totalPos = pos - num 
                                    offset = if pos == 0 then -1 else 0 in
    case totalPos `mod` 100 of
        0 -> (abs (totalPos `div` 100) + acc + 1 + offset, 0)
        newPos -> (abs (totalPos `div` 100) + acc + offset, newPos)
