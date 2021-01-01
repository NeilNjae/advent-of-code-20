-- import Debug.Trace

-- import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text hiding (take)
-- import Data.Attoparsec.Combinator
import Control.Applicative
-- import Control.Applicative.Combinators

import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))
import qualified Data.Map.Strict as M
import Data.Bool (bool)
import Data.List (delete)
import Control.Monad (guard, foldM)


type Coord = (Int, Int)
type Pixels = A.UArray Coord Bool 
type Border = A.UArray Int Bool 

data Tile = Tile 
  { tId :: Integer
  , pixels :: Pixels 
  } deriving (Show, Eq)

type Arrangement = M.Map Coord Tile


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent20.txt"
      let tiles = successfulParse text
      let arrangeRMax = (floor $ sqrt @Double $ fromIntegral $ length tiles) - 1
      let arrangement = arrangeTiles arrangeRMax tiles
      let image = assembleImage arrangeRMax arrangement
      seaMonster <- readSeaMonster
      print $ part1 arrangeRMax arrangement
      print $ part2 seaMonster image


part1 rMax arrangement 
  = product 
  $ M.elems $ M.map tId 
  $ M.filterWithKey (isCorner rMax) arrangement

part2 seaMonster image = minimum $ map (countRoughness seaMonster) transImages
  where imgTile = Tile 0 image
        transImages = map pixels $ transforms imgTile


readSeaMonster :: IO Pixels
readSeaMonster = 
  do text <- TIO.readFile "data/advent20seamonster.txt"
     return $ case parseOnly pixelsP text of
      Left  _err -> A.listArray ((0, 0), (1, 1)) []
      Right seaMonster -> seaMonster


isCorner _ (0, 0) _ = True
isCorner l (0, c) _ = c == l
isCorner l (r, 0) _ = r == l
isCorner l (r, c) _ = r == l && c == l

arrangeTiles :: Int -> [Tile] -> Arrangement
arrangeTiles rMax tiles = fst $ head $ foldM arrange (M.empty, tiles) locations
  where locations = init $ scanl nextLoc (0, 0) tiles
        nextLoc (r, c) _ = if c == rMax then (r + 1, 0) else (r, c + 1)

arrange :: (Arrangement, [Tile]) -> Coord -> [(Arrangement, [Tile])]
arrange (grid, tiles) (r, c) = 
  do  tile <- tiles
      transTile <- transforms tile
      guard $ if r == 0 then True else matchVertical tileAbove transTile
      guard $ if c == 0 then True else matchHorizontal tileLeft transTile
      return (M.insert (r, c) transTile grid, delete tile tiles)
  where tileAbove = grid M.! (r - 1 ,  c)
        tileLeft = grid M.! (r, c - 1)


matchHorizontal tile1 tile2 = (rightBorder tile1) == (leftBorder tile2)
matchVertical tile1 tile2 = (bottomBorder tile1) == (topBorder tile2)


topBorder :: Tile -> Border
topBorder Tile{..} = A.listArray (0, c1) [pixels!(0, c) | c <- [0..c1] ]
  where (_, (_, c1)) = A.bounds pixels

bottomBorder :: Tile -> Border
bottomBorder Tile{..} = A.listArray (0, c1) [pixels!(r1, c) | c <- [0..c1] ]
  where (_, (r1, c1)) = A.bounds pixels

leftBorder :: Tile -> Border
leftBorder Tile{..} = A.listArray (0, r1) [pixels!(r, 0) | r <- [0..r1] ]
  where (_, (r1, _)) = A.bounds pixels

rightBorder :: Tile -> Border
rightBorder Tile{..} = A.listArray (0, r1) [pixels!(r, c1) | r <- [0..r1] ]
  where (_, (r1, c1)) = A.bounds pixels


transforms :: Tile -> [Tile]
transforms tile = 
  [ r $ f tile
  | r <- [id, tRotate, tRotate . tRotate, tRotate . tRotate . tRotate]
  , f <- [id, tFlip]
  ]

-- rotate quarter turn clockwise
tRotate tile = tile {pixels = pixels'}
  where bs = pixels tile
        (_, (r1, c1)) = A.bounds bs
        pixels' = A.ixmap ((0, 0), (c1, r1)) rotateIndex bs
        rotateIndex (r, c) = (r1 - c, r) -- how to get to the old index from the new one

tFlip tile = tile {pixels = pixels'}
  where bs = pixels tile
        (_, (r1, c1)) = A.bounds bs
        pixels' = A.ixmap ((0, 0), (r1, c1)) flipIndex bs
        flipIndex (r, c) = (r, c1 - c) -- how to get to the old index from the new one


assembleImage :: Int -> Arrangement -> Pixels
assembleImage arrangeRMax arrangement = 
    A.array ((0,0), (imageRMax, imageRMax)) imageElements
  where (_, (tileRMax, _)) = A.bounds $ pixels $ arrangement M.! (0, 0)
        tRM1 = tileRMax - 1
        imageRMax = tRM1 * (arrangeRMax + 1) - 1
        imageElements = 
          do  ar <- [0..arrangeRMax] -- arrangement row
              ac <- [0..arrangeRMax]
              tr <- [1..tRM1]        -- tile pixels row
              tc <- [1..tRM1]
              let px = (pixels $ arrangement M.! (ar, ac)) ! (tr, tc)
              let ir = (ar * tRM1) + (tr - 1) -- assembled image row
              let ic = (ac * tRM1) + (tc - 1)
              return ((ir, ic), px)


countRoughness sm image = imPixels - (smPixels * nSeaMonsters)
  where smPixels = countPixels sm
        imPixels = countPixels image
        nSeaMonsters = length $ findSeaMonsters sm image

countPixels :: Pixels -> Int
countPixels = length . filter (== True) . A.elems

findSeaMonsters :: Pixels -> Pixels -> [Coord]
findSeaMonsters sm image = [ (r, c) 
                           | r <- [0..(imR - smR)]
                           , c <- [0..(imC - smC)]
                           , seaMonsterPresent sm image r c
                           ]
  where (_, (smR, smC)) = A.bounds sm
        (_, (imR, imC)) = A.bounds image

seaMonsterPresent sm image dr dc = all bothPresent $ A.indices sm
  where bothPresent (r, c) = if (sm!(r, c)) 
                             then (image!(r + dr, c + dc))
                             else True


showTile Tile{..} = show tId ++ "\n" ++ (showP pixels)

showP ps = unlines [[bool ' ' '\x2588' (ps!(r, c)) | c <- [0..cMax] ] | r <- [0..rMax]]
  where (_, (rMax, cMax)) = A.bounds ps
        -- sb b = bool '.' '#' b

-- -- Parse the input file

tilesP = tileP `sepBy` blankLines

blankLines = many endOfLine

tileP = Tile <$> ("Tile " *> decimal) <* ":" <* endOfLine <*> pixelsP

pixelsP = pixify <$> (pixelsRowP `sepBy` endOfLine)
pixelsRowP = many1 (satisfy (inClass " .#"))

pixify :: [String] -> Pixels
pixify rows = A.array ((0, 0), (nRows, nCols)) 
                      [ ((r, c), (rows!!r)!!c == '#')
                      | r <- [0..nRows]
                      , c <- [0..nCols]
                      ]
  where nRows = length rows - 1
        nCols = (length $ head rows) - 1


-- successfulParse :: Text -> (Integer, [Maybe Integer])
successfulParse input = 
  case parseOnly tilesP input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right tiles -> tiles
