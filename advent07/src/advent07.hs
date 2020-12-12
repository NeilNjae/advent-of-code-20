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

import Data.Maybe

data QuantifiedBag = QuantifiedBag Integer String
    deriving (Show, Eq, Ord)


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent07.txt"
      let bags = successfulParse text
      -- print bags
      -- print $ invertBags bags
      print $ part1 bags
      print $ part2 bags

part1 bags = S.size $ S.delete "shiny gold" containers
    where containers = bagsContaining (invertBags bags) (S.singleton "shiny gold") S.empty

part2 bags = (nContainedBags bags "shiny gold") - 1

invertBags bags = foldr addInvert M.empty $ concatMap swapPair $ M.assocs bags 
    where swapPair (a, bs) = [(qName b, a) | b <- S.toList bs]

addInvert :: (String, String) -> (M.Map String (S.Set String)) -> (M.Map String (S.Set String))
addInvert (k, v) m = 
    if k `M.member` m
    then M.insert k v' m
    else M.insert k (S.singleton v) m
    where v' = S.insert v (m!k)

qName (QuantifiedBag _ n) = n
qCount (QuantifiedBag n _) = n

bagsContaining :: (M.Map String (S.Set String)) -> (S.Set String) -> (S.Set String) -> (S.Set String)
bagsContaining iBags agenda result 
    | S.null agenda = result
    | otherwise = bagsContaining iBags agenda'' (S.insert thisColour result)
    where thisColour = S.findMin agenda
          agenda' = S.delete thisColour agenda
          agenda'' = if thisColour `S.member` result
                     then agenda'
                     else S.union (fromMaybe S.empty (M.lookup thisColour iBags)) agenda' 

nContainedBags :: (M.Map String (S.Set QuantifiedBag)) -> String -> Integer
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



-- successfulParse :: Text -> [[S.Set Char]]
successfulParse input = 
  case parseOnly rulesP input of
    Left  _err -> M.empty -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right bags -> bags
