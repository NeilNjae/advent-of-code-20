-- import Debug.Trace

import Text.ParserCombinators.ReadP
-- import Text.ParserCombinators.ReadP ((+++))
import Data.Char (isDigit, isAlpha)

import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict ((!))

import Data.Functor (void)
import Data.Either


data Rule = Letter Char 
           | Then [Rule]
          | Or Rule Rule
          | See Int
          deriving (Show, Eq)

type RuleSet = M.IntMap Rule


main :: IO ()
main = 
  do  text <- readFile "data/advent19.txt"
      -- print text
      let (rules, messages) = parse inputP text
      print $ part1 rules messages
      print $ part2 rules messages

setup fname = 
  do text <- readFile fname
     let (rules, messages) = parse inputP text
     let newRules = parse rulesP "8: 42 | 42 8\n11: 42 31 | 42 11 31"
     let updatedRules = M.union newRules rules
     let myParser = makeParser updatedRules (See 0)
     return (myParser, updatedRules, messages)


part1 = countMatches

part2 rules messages = countMatches updatedRules messages
  where newRules = parse rulesP "8: 42 | 42 8\n11: 42 31 | 42 11 31"
        updatedRules = M.union newRules rules

countMatches rules messages 
  = length $ filter ((== "") . snd) results
  where myParser = makeParser rules (See 0)
        results = concatMap (readP_to_S myParser) messages


parse :: ReadP a -> String -> a
parse parser str = fst $ head $ filter ((== "") . snd) $ readP_to_S parser str


-- Generate the rules

makeParser :: RuleSet -> Rule -> ReadP ()
makeParser m (Letter c) = void $ char c
makeParser m (Then rs) = mapM_ (makeParser m) rs
makeParser m (Or a b) = (makeParser m a) +++ (makeParser m b)
makeParser m (See i) = makeParser m (m!i)


-- Parse the input
inputP = (,) <$> rulesP <* blankLines <*> messagesP

rulesP = M.fromList <$> ruleP `sepBy` endOfLine
ruleP = (,) <$> decimal <* (string ": ") <*> ruleBodyP
ruleBodyP = choice [letterRuleP, orRuleP, thenRuleP, seeRuleP]

letterRuleP = Letter <$> between (string "\"") (string "\"") get
orRuleP = Or <$> thenRuleP <* (string " | ") <*> thenRuleP
thenRuleP = Then <$> seeRuleP `sepBy` (string " ")
seeRuleP = See <$> decimal

messagesP = (munch1 isAlpha) `sepBy` endOfLine

blankLines = skipMany1 endOfLine

decimal = read <$> many1 (satisfy isDigit)
endOfLine = char '\n'
