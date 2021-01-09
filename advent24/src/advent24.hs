-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text hiding (take)
-- import Data.Attoparsec.Combinator
-- import Control.Applicative
-- import Control.Applicative.Combinators

import qualified Data.Set as S
import Linear (V2(..), (^+^))
-- import Data.Semigroup
-- import Data.Monoid


data Direction = NE | E | SE | SW | W | NW
  deriving (Show, Eq, Enum, Bounded)

type Tile = V2 Int -- x, y
type Grid = S.Set Tile

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0

main :: IO ()
main = 
    do text <- TIO.readFile "data/advent24.txt"
       let walks = successfulParse text
       let grid0 = foldr flipTile S.empty walks
       print $ part1 grid0
       print $ part2 grid0

part1 grid0 = S.size grid0
part2 grid0 = S.size $ (iterate update  grid0) !! 100

delta :: Direction -> Tile
delta NE = V2  1  0
delta E  = V2  0  1
delta SE = V2 -1  1
delta SW = V2 -1  0
delta W  = V2  0 -1
delta NW = V2  1 -1


flipTile :: Tile -> Grid -> Grid
flipTile tile tiles 
  | tile `S.member` tiles = S.delete tile tiles
  | otherwise = S.insert tile tiles


neighbourSpaces :: Tile -> Grid
neighbourSpaces here = S.fromList $ map nbrSpace [minBound .. maxBound] -- [NE .. NW]
  where nbrSpace d = here ^+^ (delta d)

countOccupiedNeighbours :: Tile -> Grid -> Int
countOccupiedNeighbours cell grid = 
  S.size $ S.intersection grid $ neighbourSpaces cell

tileBecomesWhite :: Grid -> Tile -> Bool
tileBecomesWhite grid cell = black && ((nNbrs == 0) || (nNbrs > 2))
  where black = cell `S.member` grid
        nNbrs = countOccupiedNeighbours cell grid

tileBecomesBlack :: Grid -> Tile -> Bool
tileBecomesBlack grid cell = white && (nNbrs == 2)
  where white = cell `S.notMember` grid
        nNbrs = countOccupiedNeighbours cell grid

update :: Grid -> Grid
update grid = (grid `S.union` newBlacks) `S.difference` newWhites
  where neighbours = (S.foldr mergeNeighbours S.empty grid) `S.difference` grid
        mergeNeighbours cell acc = S.union acc $ neighbourSpaces cell
        newWhites = S.filter (tileBecomesWhite grid) grid
        newBlacks = S.filter (tileBecomesBlack grid) neighbours


-- Parse the input file

tilesP = tileP `sepBy` endOfLine
tileP = foldMap delta <$> many1 stepP

stepP = choice [neP, nwP, seP, swP, eP, wP]

neP = "ne" *> pure NE
nwP = "nw" *> pure NW
seP = "se" *> pure SE
swP = "sw" *> pure SW
eP  = "e"  *> pure E
wP  = "w"  *> pure W

-- successfulParse :: Text -> [Tile]
successfulParse input = 
  case parseOnly tilesP input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right tiles -> tiles
