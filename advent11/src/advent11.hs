-- import Debug.Trace

import Prelude hiding (Left, Right)

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
-- import Data.Sort
import Data.List
import Control.Monad.Reader
import Control.Monad.Loops


type Position = (Int, Int)
data Seat = Floor | Empty | Occupied deriving (Eq, Ord)
data Direction = Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft
  deriving (Eq, Ord, Show, Enum)
type Seats = M.Map Position Seat

type Neighbourhood = M.Map Position (S.Set Position)
type Rule = Seats -> Neighbourhood -> Position -> Seat -> Seat

instance Show Seat where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"


type CachedSeats a = Reader (Neighbourhood, Rule) a


main :: IO ()
main = 
  do  text <- readFile "data/advent11.txt"
      let (seats, maxCorner) = readGrid text
      -- print $ M.size seats
      -- print maxCorner
      print $ part1 seats
      print $ part2 seats


part1 seats = M.size $ M.filter (== Occupied) stableSeats
  where cachedNeighbours = allNeighbourhoods seats
        env = (cachedNeighbours, ruleA)
        stableSeats = snd $ runReader (runSteps seats) env

part2 seats = M.size $ M.filter (== Occupied) stableSeats
  where cachedNeighbours = allSightNeighbourhoods seats
        env = (cachedNeighbours, ruleB)
        stableSeats = snd $ runReader (runSteps seats) env


runSteps :: Seats -> CachedSeats (Seats, Seats)
runSteps seats = iterateUntilM (uncurry (==)) seatChanges (M.empty, seats)

seatChanges :: (Seats, Seats) -> CachedSeats (Seats, Seats)
seatChanges (_, seats0) = 
  do seats <- step seats0
     return (seats0, seats)

step :: Seats -> CachedSeats Seats
step seats = 
  do  (nbrs, rule) <- ask
      return $ M.mapWithKey (rule seats nbrs) seats

ruleA :: Seats -> Neighbourhood -> Position -> Seat -> Seat
ruleA seats nbrs here thisSeat
  | thisSeat == Empty && nOccs == 0 = Occupied
  | thisSeat == Occupied && nOccs >= 4 = Empty
  | otherwise = thisSeat
  where nOccs = M.size $ occupiedNeighbours seats nbrs here

ruleB :: Seats -> Neighbourhood -> Position -> Seat -> Seat
ruleB seats nbrs here thisSeat
  | thisSeat == Empty && nOccs == 0 = Occupied
  | thisSeat == Occupied && nOccs >= 5 = Empty
  | otherwise = thisSeat
  where nOccs = M.size $ occupiedNeighbours seats nbrs here


neighbours (r, c) = S.delete (r, c) $ S.fromList [(r + dr, c + dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1]]

neighbourhood seats here = S.intersection (M.keysSet seats) (neighbours here)

allNeighbourhoods :: Seats -> Neighbourhood
allNeighbourhoods seats = M.mapWithKey (\h _ -> neighbourhood seats h) seats

occupiedNeighbours seats nbrs here = M.filter (== Occupied) $ M.restrictKeys seats (nbrs!here)


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

allSightNeighbourhoods :: Seats -> Neighbourhood
allSightNeighbourhoods seats = M.mapWithKey (\h _ -> closestInSight seats h) seats

-- occupiedInSight :: Seats -> Position -> Seats
-- occupiedInSight seats here = M.filter (== Occupied) $ M.restrictKeys seats $ closestInSight seats here



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

