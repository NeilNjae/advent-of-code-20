-- import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text
-- import Data.Attoparsec.Combinator
import Control.Applicative

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

data QuantifiedBag = QuantifiedBag Integer String
    deriving (Show, Eq, Ord)

qName (QuantifiedBag _ n) = n
qCount (QuantifiedBag n _) = n

type Bags = S.Set String
type QBags = S.Set QuantifiedBag
type BagRules = M.Map String QBags
type InvertBags = M.Map String Bags


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent07.txt"
      let bags = successfulParse text
      -- dumpBagDot bags
      print $ part1 bags
      print $ part2 bags

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


part1 bags = S.size $ S.delete "shiny gold" containers
    where containers = bagsContaining (invertBags bags) (S.singleton "shiny gold") S.empty

part2 bags = (nContainedBags bags "shiny gold") - 1

invertBags :: BagRules -> InvertBags
invertBags bags = foldr addInvert M.empty $ concatMap swapPair $ M.assocs bags 
    where swapPair (a, bs) = [(qName b, a) | b <- S.toList bs]

addInvert :: (String, String) -> InvertBags -> InvertBags
addInvert (k, v) m = M.insert k v' m
    where v' = S.insert v (M.findWithDefault S.empty k m)


bagsContaining :: InvertBags -> Bags -> Bags -> Bags
bagsContaining iBags agenda result 
    | S.null agenda = result
    | otherwise = bagsContaining iBags agenda'' (S.insert thisColour result)
    where thisColour = S.findMin agenda
          agenda' = S.delete thisColour agenda
          agenda'' = if thisColour `S.member` result
                     then agenda'
                     else S.union (M.findWithDefault S.empty thisColour iBags) agenda' 

nContainedBags :: BagRules -> String -> Integer
nContainedBags bags thisBag = 1 + (sum $ map subCount others)
    where others = S.toList $ bags!thisBag
          subCount b = (qCount b) * (nContainedBags bags (qName b))



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
