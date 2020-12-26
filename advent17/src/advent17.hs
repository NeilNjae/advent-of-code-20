-- import Debug.Trace

import qualified Data.Set as S
import Linear (V3(..), V4(..), (^+^))

class (Num a, Ord a) => Coord a where
    (^+^^) :: a -> a -> a
    neighbourCells :: S.Set a
instance Coord (V3 Int) where
    x ^+^^ y  = x ^+^ y
    neighbourCells = S.fromList [ V3 dx dy dz
             | dx <- [-1, 0, 1]
             , dy <- [-1, 0, 1]
             , dz <- [-1, 0, 1]
             , (dx, dy, dz) /= (0, 0, 0)
             ]
instance Coord (V4 Int) where
    x ^+^^ y  = x ^+^ y
    neighbourCells = S.fromList [ V4 dx dy dz dw
             | dx <- [-1, 0, 1]
             , dy <- [-1, 0, 1]
             , dz <- [-1, 0, 1]
             , dw <- [-1, 0, 1]
             , (dx, dy, dz, dw) /= (0, 0, 0, 0)
             ]

type Grid a = S.Set a

main :: IO ()
main = 
    do grid0 <- readGrid "data/advent17.txt"
       print $ part1 grid0
       print $ part2 grid0


part1 grid0 = S.size finalGrid
  where finalGrid = head $ drop 6 $ iterate update grid0

part2 grid0 = S.size finalGrid
  where grid4 = conv34 grid0
        finalGrid = head $ drop 6 $ iterate update grid4


readGrid :: String -> IO (Grid (V3 Int))
readGrid filename = 
    do  gs <- readFile filename
        let grid = lines gs
        let isActive x y = (grid!!y)!!x == '#'
        let maxX = length (head grid) - 1
        let maxY = length grid - 1
        return $ S.fromList [ V3 x y 0 
                            | x <- [0..maxX], y <- [0..maxY], isActive x y]

conv34 :: Grid (V3 Int) -> Grid (V4 Int)
conv34 grid = S.map conv34Cell grid

conv34Cell (V3 x y z) = V4 x y z 0


neighbourSpaces :: Coord a => a -> Grid a
neighbourSpaces here = S.map (here ^+^^) neighbourCells

countOccupiedNeighbours :: Coord a => a -> Grid a -> Int
countOccupiedNeighbours cell grid = 
    S.size $ S.intersection grid $ neighbourSpaces cell

cubeSurvives :: Coord a => Grid a -> a -> Bool
cubeSurvives grid cell = alive && (nNbrs == 2 || nNbrs == 3)
  where alive = cell `S.member` grid
        nNbrs = countOccupiedNeighbours cell grid

cubeBorn :: Coord a => Grid a -> a -> Bool
cubeBorn grid cell = dead && (nNbrs == 3)
  where dead = cell `S.notMember` grid
        nNbrs = countOccupiedNeighbours cell grid

update :: Coord a => Grid a -> Grid a
update grid = S.union (S.filter (cubeSurvives grid) grid) 
                      (S.filter (cubeBorn grid) empties)
  where empties = (S.foldr mergeEmpties S.empty grid) `S.difference` grid
        mergeEmpties cell acc = S.union acc $ neighbourSpaces cell
