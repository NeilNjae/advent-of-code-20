-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text
-- import Data.Attoparsec.Combinator
import Control.Applicative

import qualified Data.Graph.DGraph as D
import qualified Data.Graph.Types as G
import qualified Data.Graph.Traversal as T

import qualified Data.Set as S
import qualified Data.Map.Strict as M

data QuantifiedBag = QuantifiedBag Int String
    deriving (Show, Eq, Ord)

type QBags = S.Set QuantifiedBag
type BagRules = M.Map String QBags
type BagGraph = D.DGraph String Int


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent07.txt"
      let bags = successfulParse text
      let graph = buildGraph bags
      -- print graph
      -- dumpBagDot bags
      print $ part1 graph
      print $ part2 graph

part1 graph = length (T.bfsVertices (D.transpose graph) "shiny gold") - 1

part2 graph = (bfsCount graph "shiny gold") - 1

bfsCount :: BagGraph -> String -> Int
bfsCount graph thisBag = 1 + (sum $ map subCount others)
    where others = D.outboundingArcs graph thisBag
          subCount a = (G.attribute a) * (bfsCount graph $ G.destinationVertex a)


buildGraph :: BagRules -> BagGraph
buildGraph rules = M.foldrWithKey addRule G.empty rules

addRule :: String -> QBags -> BagGraph -> BagGraph
addRule source dests graph = S.foldr (addArc source) graph dests

addArc :: String -> QuantifiedBag -> BagGraph -> BagGraph
addArc source (QuantifiedBag quantity destination) graph = D.insertArc arc graph
  where arc = G.Arc source destination quantity


-- dumpBagDot bags = 
--     do writeFile "a07dump.dot" "digraph {\n"
--        mapM_ dumpABag (M.assocs bags)
--        appendFile "a07dump.dot" "shiny_gold [fillcolor = gold1 ]\n"
--        appendFile "a07dump.dot" "}\n"

-- dumpABag (bag, contents) =
--     mapM_ (dumpALink bag) (S.toList contents)

-- dumpALink bag (QuantifiedBag n name) =
--     do  let name' = squashName name
--         let bag' = squashName bag
--         let txt = bag' ++ " -> " ++ name' ++ "\n"
--         appendFile "a07dump.dot" txt

-- squashName :: String -> String
-- squashName name = [if c == ' ' then '_' else c | c <- name]


-- -- Parse the input file

bagNameP = manyTill anyChar ((string " bags") <|> (string " bag"))

quantifiedBagP = QuantifiedBag <$> decimal <* space <*> bagNameP

emptyBagListP = "no other bags" *> pure S.empty

bagListP = S.fromList <$> sepBy quantifiedBagP (string ", ")

bagContentsP = emptyBagListP <|> bagListP

ruleP = (,) <$> bagNameP <* " contain " <*> bagContentsP <* "."

rulesP = M.fromList <$> sepBy ruleP endOfLine


successfulParse :: Text -> BagRules
successfulParse input = 
  case parseOnly rulesP input of
    Left  _err -> M.empty -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right bags -> bags
