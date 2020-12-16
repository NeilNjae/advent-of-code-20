-- import Debug.Trace

import qualified Data.Map as M
-- import qualified Data.Set as S
import Data.List

main :: IO ()
main = 
    do  
        numStrs <- readFile "data/advent10.txt"
        let nums = map (read @Int) $ lines numStrs
        let device = 3 + maximum nums
        let sortedNums = sort (0:device:nums)
        print $ part1 sortedNums
        print $ part2 sortedNums

part1 :: [Int] -> Int
part1 nums = jolt1s * jolt3s
    where jolt1s = joltNs 1 nums
          jolt3s = joltNs 3 nums

joltNs d ns = length $ filter diff $ zip ns (tail ns)
    where diff (a, b) = (b - a) == d

part2 :: [Int] -> Int
part2 nums = snd $ M.findMax $ foldl includeAdapter (M.singleton 0 1) $ tail nums

includeAdapter arrangements adapter = M.insert adapter earlierCount arrangements
    where 
        earlierArrangements = M.filterWithKey (\k _ -> (adapter - k) <= 3) arrangements
        earlierCount = M.foldl (+) 0 earlierArrangements
