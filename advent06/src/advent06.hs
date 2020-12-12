-- import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text
-- import Data.Attoparsec.Combinator

import qualified Data.Set as S

main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent06.txt"
      let groups = successfulParse text
      -- print groups
      print $ part1 groups
      print $ part2 groups

part1 groups = sum $ map (S.size . S.unions) groups
part2 groups = sum $ map (S.size . intersections) groups

intersections :: Ord a => [S.Set a] -> S.Set a
intersections group = foldl S.intersection (head group) group

-- -- Parse the input file

blankLines = skipMany1 endOfLine

personP = S.fromList <$> many1 letter
groupP = sepBy1 personP endOfLine

groupsP = sepBy1 groupP blankLines

-- successfulParse :: Text -> [Passport]
successfulParse input = 
  case parseOnly groupsP input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right groups -> groups
