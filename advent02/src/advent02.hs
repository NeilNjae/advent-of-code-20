-- import Debug.Trace

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

data Policy = Policy
  { lower :: Int
  , upper :: Int
  , character :: Char
  , password :: String    
  } deriving (Show, Eq, Ord)

main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent02.txt"
      let policies = successfulParse text
      print $ part1 policies
      print $ part2 policies
      -- print $ head $ part2 nums

part1 = length . filter inRange
  where nCharsPresent p = length $ filter (== (character p)) (password p)
        inRange p = (nCharsPresent p >= (lower p)) && (nCharsPresent p <= (upper p))

part2 = length . filter matches

matches p = ((pw!!l) == c) /= ((pw!!u) == c)
  where pw = password p
        c = character p
        l = (lower p) - 1
        u = (upper p) - 1


-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integerP = lexeme L.decimal
-- signedInteger = L.signed sc integer
symb = L.symbol sc
hyphenP = symb "-"
colonP = symb ":"
characterP = alphaNumChar <* sc
passwordP = some alphaNumChar <* sc

policiesP = many policyP
policyP = Policy <$> integerP <* hyphenP <*> integerP <*> characterP <* colonP <*> passwordP
-- policyP = (,,,) <$> integerP <* hyphenP <*> integerP <*> characterP <* colonP <*> passwordP

-- successfulParse :: Text -> [Policy]
successfulParse input = 
  case parse policiesP "input" input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right policies -> policies
