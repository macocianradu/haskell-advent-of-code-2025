module Main (main) where

import System.Environment
import qualified Data.Map as M

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let papers = lines input
    let env = let maxY = length papers - 1
                  maxX = length (papers !! 0) - 1 in 
                  Env {
                      maxY = maxY,
                      maxX = maxX,
                      items = M.fromList $ zip (generateIndexes (maxY, maxX)) $ concat papers
                  }
    let initialPapers = length $ filter paperFilter $ M.toList $ items env
    print $ length $ filter (isAccessible env) $ generateIndexes (maxY env, maxX env)
    let finalEnv = removeAll env
    let finalPapers = length $ filter paperFilter $ M.toList $ items finalEnv
    print $ initialPapers - finalPapers

data Env = Env 
    { maxY :: Int
    , maxX :: Int
    , items :: M.Map (Int, Int) Char
    } deriving (Show, Eq)

paperFilter :: ((Int,Int),Char) -> Bool
paperFilter (_,'@') = True
paperFilter (_,_) = False

generateIndexes :: (Int, Int) -> [(Int, Int)]
generateIndexes (maxY, maxX) = [(y, x) | y <- [0.. maxY], x <- [0.. maxX]]

isPaper :: Env -> (Int, Int) -> Bool
isPaper env (y, x)
    | y >= 0 && y <= maxY env && x >= 0 && x <= maxX env = (items env) M.! (y, x) == '@'
    | otherwise = False

isAccessible :: Env -> (Int, Int) -> Bool
isAccessible env (y, x) 
    | isPaper env (y, x) = let papers = filter (isPaper env) [(y-1, x-1), (y, x-1), (y+1,x-1), (y-1,x), (y+1,x), (y-1,x+1), (y,x+1), (y+1,x+1)] in 
        length papers < 4
    | otherwise = False

removePapers :: Env -> [(Int, Int)] -> Env
removePapers env [] = env
removePapers env (x:xs) = removePapers env{items = (M.insert x '.' (items env))} xs

removeAll :: Env -> Env
removeAll env = let accessible = filter (isAccessible env) $ generateIndexes (maxY env, maxX env) in
    if length accessible > 0
        then removeAll (removePapers env accessible)
        else env
