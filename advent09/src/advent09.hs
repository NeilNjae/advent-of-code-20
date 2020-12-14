-- import Debug.Trace

import Data.List

main :: IO ()
main = 
    do  numStrs <- readFile "data/advent09.txt"
        let nums = map (read @Int) $ lines numStrs
        let firstInvalid = part1 nums
        print firstInvalid
        print $ part2 firstInvalid nums
        -- print $ head $ part2 nums

part1 nums = fst $ head $ filter (not . valid) $ slidingWindow 25 nums

slidingWindow winSize nums = 
    zip (drop winSize nums) $ map (take winSize) $ tails nums

valid (target, window) = not $ null [(x, y) | x <- window, y <- window, x + y == target]


part2 target nums = (maximum section) + (minimum section)
    where section = head $ filter (sumsToTarget target) $ subStrings nums

-- subStrings :: [a] -> [[a]]
subStrings = (concatMap inits) . tails

-- sumsToTarget :: Int -> [Int] -> Bool
sumsToTarget target ns = sum ns == target

