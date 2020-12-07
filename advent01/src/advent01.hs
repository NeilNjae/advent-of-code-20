-- import Debug.Trace


main :: IO ()
main = 
    do  numStrs <- readFile "data/advent01.txt"
        let nums = map (read @Int) $ lines numStrs
        print $ head $ part1 nums
        print $ head $ part2 nums

part1 nums = [ x * y 
             | x <- nums
             , y <- nums
             , x < y
             , x + y == 2020
             ]

part2 nums = [ x * y * z
             | x <- nums
             , y <- nums
             , z <- nums
             , x < y
             , y < z
             , x + y + z == 2020
             ]