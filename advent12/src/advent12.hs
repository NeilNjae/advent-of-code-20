-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text
-- import Data.Attoparsec.Combinator
-- import Control.Applicative


data Direction = North | East | South | West 
  deriving (Show, Eq, Ord, Enum, Bounded)

type Position = (Int, Int) -- (x, y)

data Action a = N a | S a | E a | W a | L a | R a | F a
  deriving (Show, Eq, Ord)

-- data Ship = Ship { direction :: Direction, position :: Position }
--   deriving (Show, Eq, Ord)

data Ship = Ship { position :: Position 
                 , direction :: Direction
                 , waypoint :: Position
                 }
  deriving (Show, Eq, Ord)



main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent12.txt"
      let actions = successfulParse text
      -- print actions
      print $ part1 actions
      print $ part2 actions


part1 actions = manhattan (position ship1) start
  where start = (0, 0)
        ship0 = Ship {position = start, direction = East, waypoint = (10, 1)}
        ship1 = foldl act ship0 actions

part2 actions = manhattan (position ship1) start
  where start = (0, 0)
        ship0 = Ship {position = start, direction = East, waypoint = (10, 1)}
        ship1 = foldl actW ship0 actions

-- apAc actions = ship1
--   where start = (0, 0)
--         ship0 = Ship {position = start, direction = East }
--         ship1 = foldl act ship0 actions

-- apAcW actions = ship1
--   where start = (0, 0)
--         ship0 = ShipW {positionW = start, waypoint = (10, 1) }
--         ship1 = foldl actW ship0 actions

act :: Ship -> Action Int -> Ship
act Ship{..} (N d) = Ship { position = dDelta d North position, ..}
act Ship{..} (S d) = Ship { position = dDelta d South position, ..}
act Ship{..} (W d) = Ship { position = dDelta d West  position, ..}
act Ship{..} (E d) = Ship { position = dDelta d East  position, ..}
act Ship{..} (L a) = Ship { direction = d, ..} where d = (iterate predW direction) !! (a `div` 90)
act Ship{..} (R a) = Ship { direction = d, ..} where d = (iterate succW direction) !! (a `div` 90)
act Ship{..} (F d) = Ship { position = dDelta d direction position, ..} 


actW Ship{..} (N d) = Ship { waypoint = dDelta d North waypoint, ..}
actW Ship{..} (S d) = Ship { waypoint = dDelta d South waypoint, ..}
actW Ship{..} (W d) = Ship { waypoint = dDelta d West  waypoint, ..}
actW Ship{..} (E d) = Ship { waypoint = dDelta d East  waypoint, ..}
actW Ship{..} (L a) = Ship { waypoint = d, ..} where d = (iterate rotL waypoint) !! (a `div` 90)
actW Ship{..} (R a) = Ship { waypoint = d, ..} where d = (iterate rotR waypoint) !! (a `div` 90)
actW Ship{..} (F d) = Ship { position = p', ..} 
  where (x, y) = position
        (dx, dy) = waypoint
        p' = (x + (d* dx), y + (d * dy))

rotL (x, y) = (-y, x)
rotR (x, y) = (y, -x)

delta North = ( 0,  1)
delta South = ( 0, -1)
delta East  = ( 1,  0)
delta West  = (-1,  0)

dDelta dist dir (x, y) = (x + (dist * dx), y + (dist * dy))
  where (dx, dy) = delta dir

manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


-- | a `succ` that wraps 
succW :: (Bounded a, Enum a, Eq a) => a -> a 
succW dir | dir == maxBound = minBound
          | otherwise = succ dir

-- | a `pred` that wraps
predW :: (Bounded a, Enum a, Eq a) => a -> a
predW dir | dir == minBound = maxBound
          | otherwise = pred dir

-- Parse the input file

actionsP = actionP `sepBy` endOfLine

actionP = choice [nP, sP, eP, wP, lP, rP, fP]

nP = N <$> ("N" *> decimal)
sP = S <$> ("S" *> decimal)
eP = E <$> ("E" *> decimal)
wP = W <$> ("W" *> decimal)
lP = L <$> ("L" *> decimal)
rP = R <$> ("R" *> decimal)
fP = F <$> ("F" *> decimal)

successfulParse :: Text -> [Action Int]
successfulParse input = 
  case parseOnly actionsP input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right actions -> actions
