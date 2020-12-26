-- import Debug.Trace

import qualified Data.Set as S
import Linear (V3(..), V4(..), (^+^), (^-^))
import qualified Data.Vector as V

type Coord = V3 Int -- x, y, z
type Grid = S.Set Coord

main :: IO ()
main = 
    do grid0 <- readGrid "data/advent17.txt"
       print grid0
       let finalGrid = head $ drop 6 $ iterate update grid0
       print $ S.size finalGrid


readGrid :: String -> IO Grid
readGrid filename = 
    do  gs <- readFile filename
        let grid = lines gs
        let isActive x y = (grid!!y)!!x == '#'
        let maxX = length (head grid) - 1
        let maxY = length grid - 1
        return $ S.fromList [ V3 x y 0 | x <- [0..maxX], y <- [0..maxY], isActive x y]

neighbourSpaces :: Coord -> Grid
neighbourSpaces here = S.map (here ^+^) nbrs
  where nbrs = S.fromList [ V3 dx dy dz
             | dx <- [-1, 0, 1]
             , dy <- [-1, 0, 1]
             , dz <- [-1, 0, 1]
             , (dx, dy, dz) /= (0, 0, 0)]

countOccupiedNeighbours :: Coord -> Grid -> Int
countOccupiedNeighbours cell grid = S.size $ S.intersection grid $ neighbourSpaces cell

cubeSurvives :: Grid -> Coord -> Bool
cubeSurvives grid cell = alive && (nNbrs == 2 || nNbrs == 3)
  where alive = cell `S.member` grid
        nNbrs = countOccupiedNeighbours cell grid

cubeBorn :: Grid -> Coord -> Bool
cubeBorn grid cell = dead && (nNbrs == 3)
  where dead = cell `S.notMember` grid
        nNbrs = countOccupiedNeighbours cell grid

update :: Grid -> Grid
update grid = S.union (S.filter (cubeSurvives grid) grid) (S.filter (cubeBorn grid) empties)
  where empties = (S.foldr mergeEmpties S.empty grid) `S.difference` grid
        mergeEmpties cell acc = S.union acc $ neighbourSpaces cell
