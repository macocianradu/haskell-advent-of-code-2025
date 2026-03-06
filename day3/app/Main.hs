module Main (main) where

import System.Environment
import Data.Char (digitToInt)

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let banks = lines input
    let values = map (getDigits 12) banks
    let numberValues = map digitsToNumber values
    print numberValues
    print $ sum numberValues

digitsToNumber :: [Int] -> Int
digitsToNumber [] = 0
digitsToNumber [x] = x
digitsToNumber (x:xs) = x * 10 ^ (length xs) + digitsToNumber xs
-- 
-- highestDigits :: (Int, Int) -> [Char] -> (Int, Int)
-- highestDigits (d1, d2) [] = (d1, d2)
-- highestDigits (d1, d2) [h] = let digit = digitToInt h in (d1, if digit > d2 then digit else d2)
-- highestDigits (d1, d2) (h:t)
--     | digitToInt h > d1 = highestDigits (digitToInt h, 0) t
--     | digitToInt h > d2 = highestDigits (d1, digitToInt h) t
--     | otherwise = highestDigits (d1, d2) t
--

getDigits :: Int -> [Char] -> [Int]
getDigits n str = getDigitsInternal [] n $ map digitToInt str

getDigitsInternal :: [Int] -> Int -> [Int] -> [Int]
getDigitsInternal acc 0 _ = acc
getDigitsInternal acc n lst = let nxt = highestDigitBeforeN (0, 0) (1, length lst - n + 1) lst in
    getDigitsInternal (acc ++ [fst nxt]) (n - 1) (drop (snd nxt) lst)
    

highestDigitBeforeN :: (Int, Int) -> (Int, Int) -> [Int] -> (Int, Int)
highestDigitBeforeN acc _ [] = acc
highestDigitBeforeN (maxDigit, maxPos) (pos, n) (x:xs)
    | pos <= n = if x > maxDigit
        then highestDigitBeforeN (x, pos) (pos + 1, n) xs
        else highestDigitBeforeN (maxDigit, maxPos) (pos + 1, n) xs
    | otherwise = (maxDigit, maxPos)
