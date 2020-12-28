-- import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

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
  do  text <- TIO.readFile "data/advent19b.txt"
      -- print text
      let (rules, messages) = successfulParse text
      let messagesT = map T.pack messages
      -- print rules
      -- print messages
      print $ part1 rules messagesT
      print $ part2 rules messagesT
      -- print $ part2 text

setup fname = 
  do text <- TIO.readFile fname
     let (rules, messages) = successfulParse text
     let messagesT = map T.pack messages
     let Right newRules = parse rulesP "rules" "8: 42 | 42 8\n11: 42 31 | 42 11 31"
     let updatedRules = M.union newRules rules
     let myParser = (makeParser updatedRules (See 0)) -- <* eof
     return (myParser, updatedRules, messagesT)



part1 = countMatches

part2 rules messages = countMatches updatedRules messages
  where Right newRules = parse rulesP "rules" "8: 42 | 42 8\n11: 42 31 | 42 11 31"
        updatedRules = M.union newRules rules

countMatches rules messages 
  = length 
  $ filter isRight 
  $ map (parse myParser "message") messages
  where myParser = (makeParser rules (See 0)) -- <* eof

prettyResults rs = map p rs
  where p (Left e) = errorBundlePretty e
        p (Right r) = "^" ++ show r


-- Generate the rules

makeParser :: RuleSet -> Rule -> Parser ()
makeParser m (Letter c) = void $ char c
makeParser m (Then rs) = mapM_ (makeParser m) rs
makeParser m (Or a b) = (try (makeParser m a)) <|> (makeParser m b)
makeParser m (See i) = makeParser m (m!i)


-- Parse the input

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc
colonP = symb ":"
pipeP = symb "|"
quoteP = symb "\""


rulesP = M.fromList <$> ruleP `sepEndBy` newline
ruleP = (,) <$> integer <* colonP <*> ruleBodyP
ruleBodyP = choice [(try letterRuleP), (try orRuleP), (try thenRuleP), (try seeRuleP)]

letterRuleP = Letter <$> between quoteP quoteP letterChar
orRuleP = Or <$> thenRuleP <* pipeP <*> thenRuleP
thenRuleP = Then <$> some seeRuleP
seeRuleP = See <$> integer


inputP = (,) <$> rulesP <* (some newline) <*> messagesP

messagesP = messageP `sepBy` newline
messageP = some letterChar


-- successfulParse :: Text -> (Integer, [Maybe Integer])
successfulParse input = 
  case parse inputP "input" input of
    Left  _err -> (M.empty, []) -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right expressions -> expressions
