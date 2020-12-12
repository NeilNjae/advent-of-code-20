-- import Debug.Trace

import Numeric
import Data.List

main :: IO ()
main = 
  do  text <- readFile "data/advent05.txt"
      let passes = lines text
      print $ part1 passes
      print $ part2 passes

part1 = maximum . map convert

part2 passes = head $ expecteds \\ ns
  where ns = map convert passes
        highest = maximum ns
        lowest  = minimum ns
        expecteds = [lowest..highest]

directionToInt :: Char -> Int
directionToInt dir = if dir `elem` "BR" then 1 else 0

convert :: String -> Int
convert = fst . head . readInt 2 (`elem` "FBLR") directionToInt
