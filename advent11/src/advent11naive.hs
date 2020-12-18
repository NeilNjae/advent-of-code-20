-- import Debug.Trace

import Prelude hiding (Left, Right)

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
-- import Data.Sort
import Data.List

type Position = (Int, Int)
data Seat = Floor | Empty | Occupied deriving (Eq, Ord)
data Direction = Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft
  deriving (Eq, Ord, Show, Enum)
type Seats = M.Map Position Seat

instance Show Seat where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"


main :: IO ()
main = 
  do  text <- readFile "data/advent11.txt"
      let (seats, maxCorner) = readGrid text
      print $ M.size seats
      print maxCorner
      print $ part1 seats
      print $ part2 seats
      -- print $ part2 trees maxCorner


part1 seats = M.size $ M.filter (== Occupied) $ runUntilSame ruleA seats
part2 seats = M.size $ M.filter (== Occupied) $ runUntilSame ruleB seats

step rule seats = M.mapWithKey (rule seats) seats

runSteps rule seats = iterate (step rule) seats

seatDifferences rule seats = zip (runSteps rule seats) (tail $ runSteps rule seats)

runUntilSame rule seats = fst $ head $ dropWhile (uncurry (/=)) $ seatDifferences rule seats


ruleA seats here thisSeat
  | thisSeat == Empty && nOccs == 0 = Occupied
  | thisSeat == Occupied && nOccs >= 4 = Empty
  | otherwise = thisSeat
  where nOccs = M.size $ occupiedNeighbours seats here

ruleB seats here thisSeat
  | thisSeat == Empty && nOccs == 0 = Occupied
  | thisSeat == Occupied && nOccs >= 5 = Empty
  | otherwise = thisSeat
  where nOccs = M.size $ occupiedInSight seats here


neighbours (r, c) = S.delete (r, c) $ S.fromList [(r + dr, c + dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1]]

neighbourhood seats here = M.restrictKeys seats (neighbours here)
occupiedNeighbours seats here = M.filter (== Occupied) $ neighbourhood seats here


onSightLine :: Position -> Direction -> Position -> Bool
onSightLine (r0, c0) Down      (r, c) = (c0 == c) && (r > r0)
onSightLine (r0, c0) Up        (r, c) = (c0 == c) && (r < r0)
onSightLine (r0, c0) Right     (r, c) = (r0 == r) && (c > c0)
onSightLine (r0, c0) Left      (r, c) = (r0 == r) && (c < c0)
onSightLine (r0, c0) DownRight (r, c) = ((r - r0) > 0) && ((r - r0) == (c - c0))
onSightLine (r0, c0) UpLeft    (r, c) = ((r - r0) < 0) && ((r - r0) == (c - c0))
onSightLine (r0, c0) DownLeft  (r, c) = ((r - r0) > 0) && ((r - r0) == (c0 - c))
onSightLine (r0, c0) UpRight   (r, c) = ((r - r0) < 0) && ((r - r0) == (c0 - c))

manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

closestInDirection seats here direction = take 1 sortedSeats
  -- where seatsInDirection = M.keys $ M.filterWithKey (\o _ -> onSightLine here direction o) seats
  where seatsInDirection = filter (onSightLine here direction) $ M.keys seats
        sortedSeats = sortOn (manhattan here) seatsInDirection 

closestInSight :: Seats -> Position -> (S.Set Position)
closestInSight seats here = S.fromList $ concatMap (closestInDirection seats here) [d | d <- [Up .. UpLeft]]

occupiedInSight :: Seats -> Position -> Seats
occupiedInSight seats here = M.filter (== Occupied) $ M.restrictKeys seats $ closestInSight seats here


readGrid :: String -> (Seats, Position)
readGrid input = (seats, (maxR, maxC))
  where seats = M.fromList $ concat 
                  [ [((r, c), Empty) | (t, c) <- zip row [0..], t == 'L']
                  | (row, r) <- zip rows [0..] ]
        rows = lines input
        maxC = (length $ head rows) - 1
        maxR = (length rows) - 1

showGrid seats (maxR, maxC) = 
  unlines $ [ concat [showSeat (r, c) | c <- [0..maxC] ] | r <- [0..maxR]]
  where showSeat here = show $ M.findWithDefault Floor here seats

