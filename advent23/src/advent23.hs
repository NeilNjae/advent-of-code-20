-- import Debug.Trace

import qualified Data.List.PointedList.Circular as P
import Control.Lens
import Data.Maybe
import Data.Char (digitToInt)

import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as V


puzzleInput :: [Int]
puzzleInput = map digitToInt "538914762"
-- puzzleInput = map digitToInt "389125467" -- example

main :: IO ()
main = 
  do  putStrLn $ part1 puzzleInput
      print $ part2 puzzleInput


part1 nums = label $ playN cups 100
  where cups = fromJust $ P.fromList nums

part2 nums = a * b
  where finalCups = runGame nums 1000000 10000000
        (a, b) = clockwiseOf1 finalCups


label cups = concatMap show $ tail $ takeRight (P.length cups) atOne
  where atOne = fromJust $ P.find 1 cups

playN cups n = (iterate playOne cups) !! n

playOne cups = P.next replacedAtCurrent
  where current = cups ^. P.focus
        held = takeRight 3 $ P.next cups
        shorter = fromJust $ dropRight 3 $ P.next cups
        destination = validDestination (current - 1) 9 held
        shorterAtDestination = fromJust $ P.find destination shorter
        replaced = foldr P.insertRight shorterAtDestination $ reverse held
        replacedAtCurrent = fromJust $ P.find current replaced


validDestination 0 max missing = validDestination max max missing
validDestination n max missing
  | n `elem` missing = validDestination (n - 1) max missing
  | otherwise = n


takeRight :: Int -> P.PointedList a -> [a]
takeRight 0 _ = []
takeRight n xs = (xs ^. P.focus):(takeRight (n - 1) $ P.next xs)

dropRight :: Int -> P.PointedList a -> Maybe (P.PointedList a)
dropRight 0 xs = Just xs
dropRight n xxs = case (P.deleteRight xxs) of
  Just xs -> dropRight (n - 1) xs
  Nothing -> Nothing


clockwiseOf1 cups = (a, b)
  where a = cups!!1
        b = cups!!a

runGame :: [Int] -> Int -> Int -> [Int]
runGame seed cupsNeeded roundsNeeded =
  runST $ 
    do cups <- seedGame seed cupsNeeded
       gameLoop roundsNeeded cupsNeeded cups
       mapM (V.read cups) [0..cupsNeeded] 

seedGame :: [Int] -> Int -> ST s (V.MVector s Int)
seedGame seed cupsNeeded = 
  do  cups <- V.new (cupsNeeded + 1)
      let extended = seed ++ [10, 11..]
      forM_ [0..cupsNeeded] $ \i -> V.write cups i (i + 1)
      forM_ (zip seed $ tail extended) $ \(i, j) -> V.write cups i j
      V.write cups 0 (head seed)
      let end = if cupsNeeded > (length seed)
                then cupsNeeded
                else last seed
      V.write cups end (head seed)
      return cups

gameLoop targetRound maxCups cups =
    do forM_ [1..targetRound]
             (\_ -> gameStep maxCups cups)
       return ()

gameStep :: Int -> V.MVector s Int -> ST s ()
gameStep maxCups cups = 
  do  current <- V.read cups 0
      held1 <- V.read cups current
      held2 <- V.read cups held1
      held3 <- V.read cups held2
      afterHeld <- V.read cups held3

          -- close the loop, removing the held cups
      V.write cups current afterHeld 

      let destination = 
            validDestination (current - 1) maxCups [held1, held2, held3]
      afterDestination <- V.read cups destination

          -- make the held come after the destination
      V.write cups destination held1 

          -- make the end of the held point into the rest of the loop
      V.write cups held3 afterDestination

          -- advance current
      nextCup <- V.read cups current
          -- and store it
      V.write cups 0 nextCup
      return ()

