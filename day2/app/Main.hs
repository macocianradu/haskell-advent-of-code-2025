module Main (main) where

import Data.List.Split
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let ranges =  concat $ map parseRange $ splitOn "," input
    let invalidNumbers = filter (not . isValid) ranges
    print invalidNumbers 
    print $ sum invalidNumbers 

parseRange :: String -> [Int]
parseRange rng = let s = splitOn "-" rng in
    [read $ s !! 0..read $ s !! 1]

isValid :: Int -> Bool
isValid numb = let str = show numb in
    not $ equalSplits 1 str
    

equalSplits :: Int -> String -> Bool
equalSplits sp str = case sp > length str `div` 2 of
    True -> False
    False -> if allEqual (chunksOf sp str) then True else equalSplits (sp + 1) str

allEqual :: [String] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:x2:t) = if x == x2 then allEqual (x2:t) else False
