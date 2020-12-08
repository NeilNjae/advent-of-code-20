-- import Debug.Trace

import qualified Data.Set as S

type Position = (Int, Int)
type Trees = S.Set Position

main :: IO ()
main = 
  do  text <- readFile "data/advent03.txt"
      let (trees, maxCorner) = readGrid text
      -- print $ S.size trees
      -- print trees
      -- print $ maxCorner
      -- print $ take 25 $ visitedPlaces (1, 3) maxCorner
      -- print $ takeWhile (withinTrees maxCorner) $ visitedPlaces (1, 3) maxCorner
      print $ part1 trees maxCorner
      print $ part2 trees maxCorner


readGrid :: String -> (Trees, Position)
readGrid input = ( S.fromList [ (r, c) 
                              | r <- [0..maxR], c <- [0..maxC]
                              , (grid!!r)!!c == '#']
                 , (maxR, maxC)
                 )
    where grid = lines input
          maxC = (length $ head grid) - 1
          maxR = (length grid) - 1


part1 trees maxCorner = countEncounteredTrees trees maxCorner (1, 3)

part2 trees maxCorner = foldr1 (*) $ map cet [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  where cet = countEncounteredTrees trees maxCorner

countEncounteredTrees trees maxCorner delta = S.size $ S.intersection trees visited
  where visited = S.fromList $ takeWhile (withinTrees maxCorner) $ visitedPlaces delta maxCorner


visitedPlaces :: Position -> Position -> [Position]
visitedPlaces (dr, dc) (_maxR, maxC) = iterate wrappingStep (0, 0)
  where wrappingStep (r, c) = (r + dr, (c + dc) `mod` (maxC + 1))

withinTrees (maxR, maxC) (r, c) = r >= 0 && r <= maxR && c >= 0 && c <= maxC

