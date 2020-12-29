-- import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text 
-- import Data.Attoparsec.Combinator
import Control.Applicative
-- import Control.Applicative.Combinators

import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict ((!))
import Data.Functor (void)
import Prelude hiding (take)
import Data.Either 


data Rule = Letter Char 
          | Then [Rule]
          | Or Rule Rule
          | See Int
          deriving (Show, Eq)


type RuleSet = M.IntMap Rule


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent19.txt"
      -- print text
      let (rules, messages) = successfulParse inputP text
      let messagesT = map T.pack messages
      -- TIO.writeFile "rules19.atto.txt" $ T.pack $ show rules
      print $ length rules
      print $ length messages
      print $ part1 rules messagesT
      print $ part2 rules messagesT

setup fname = 
  do text <- TIO.readFile fname
     let (rules, messages) = successfulParse inputP text
     let messagesT = map T.pack messages
     let Right newRules = parseOnly rulesP "8: 42 | 42 8\n11: 42 31 | 42 11 31"
     let updatedRules = M.union newRules rules
     let myParser = (makeParser updatedRules (See 0)) <* endOfInput
     return (myParser, updatedRules, messagesT)



part1 = countMatches

part2 rules messages = countMatches updatedRules messages
  where Right newRules = parseOnly rulesP "8: 42 | 42 8\n11: 42 31 | 42 11 31"
        updatedRules = M.union newRules rules

countMatches rules messages 
  = length 
  $ filter isRight 
  $ map (parseOnly myParser) messages
  where myParser = ((makeParser rules (See 0)) <* endOfInput)


-- Generate the rules

makeParser :: RuleSet -> Rule -> Parser ()
makeParser m (Letter c) = void $ char c
makeParser m (Then rs) = mapM_ (\r -> try (makeParser m r)) rs
makeParser m (Or a b) = (try (makeParser m a)) <|> (makeParser m b)
makeParser m (See i) = makeParser m (m!i)


-- Parse the input

rulesP = M.fromList <$> ruleP `sepBy` endOfLine
ruleP = (,) <$> decimal <* ": " <*> ruleBodyP
ruleBodyP = choice [letterRuleP, orRuleP, thenRuleP, seeRuleP]

letterRuleP = Letter <$> ("\"" *> anyChar) <* "\""
orRuleP = Or <$> thenRuleP <* " | " <*> thenRuleP
thenRuleP = Then <$> seeRuleP `sepBy` (string " ")
seeRuleP = See <$> decimal


inputP = (,) <$> rulesP <* blankLines <*> messagesP

messagesP = (many1 letter) `sepBy` endOfLine

blankLines = skipMany1 endOfLine


-- successfulParse :: Text -> (Integer, [Maybe Integer])
successfulParse parser input = 
  case parseOnly parser input of
    Left  _err -> (M.empty, []) -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right expressions -> expressions
