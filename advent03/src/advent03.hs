-- import Debug.Trace

import qualified Data.Set as S

type Position = (Int, Int)
type Trees = S.Set Position

main :: IO ()
main = 
  do  text <- readFile "data/advent03.txt"
      let (trees, maxCorner) = readGrid text
      print $ part1 trees maxCorner
      print $ part2 trees maxCorner


readGrid :: String -> (Trees, Position)
readGrid input = (trees, (maxR, maxC))
  where trees = S.fromList $ concat 
                  [ [(r, c) | (t, c) <- zip row [0..], t == '#']
                  | (row, r) <- zip rows [0..] ]
        rows = lines input
        maxC = (length $ head rows) - 1
        maxR = (length rows) - 1

part1 trees maxCorner = countEncounteredTrees trees maxCorner (1, 3)

part2 trees maxCorner = product $ map cet [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  where cet = countEncounteredTrees trees maxCorner

countEncounteredTrees trees maxCorner delta = S.size $ S.intersection trees visited
  where visited = S.fromList $ takeWhile (withinTrees maxCorner) $ visitedPlaces delta maxCorner

visitedPlaces :: Position -> Position -> [Position]
visitedPlaces (dr, dc) (_maxR, maxC) = iterate wrappingStep (0, 0)
  where wrappingStep (r, c) = (r + dr, (c + dc) `mod` (maxC + 1))

withinTrees (maxR, maxC) (r, c) = r >= 0 && r <= maxR && c >= 0 && c <= maxC
