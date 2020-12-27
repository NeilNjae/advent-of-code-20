-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text hiding (take)
-- import Data.Attoparsec.Combinator
import Control.Applicative
-- import Control.Applicative.Combinators

data Expression 
  = Number Integer
  | Mul Expression Expression
  | Add Expression Expression
  deriving (Show, Eq)


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent18.txt"
      print $ part1 text
      print $ part2 text


part1 = parseAndEval expressionsP
part2 = parseAndEval pExpressionsP

parseAndEval parser text = sumEval $ successfulParse parser text

sumEval = sum . map evaluate

showE (Number x) = show x
showE (Mul a b) = "(" ++ showE a ++ " * " ++ showE b ++ ")"
showE (Add a b) = "(" ++ showE a ++ " + " ++ showE b ++ ")"

evaluate (Number x)  = x
evaluate (Mul a b) = (evaluate a) * (evaluate b)
evaluate (Add a b) = (evaluate a) + (evaluate b)


-- Parse the input file

numberP = Number <$> decimal
mulExprP = string " * " *> pure Mul
addExprP = string " + " *> pure Add


-- Part 1 parsing

expressionsP = exprP `sepBy` endOfLine

exprP = elementP `chainl1` (mulExprP <|> addExprP)

elementP = numberP <|> bracketedP
bracketedP = "(" *> exprP <* ")"


-- Part 2 parsing

pExpressionsP = pExprP `sepBy` endOfLine

pExprP = pTermP `chainl1` mulExprP
pTermP = pElementP `chainl1` addExprP

pElementP = numberP <|> pBracketedP
pBracketedP = "(" *> pExprP <* ")"


-- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- chainl1 p op = p >>= rest
--     where 
--         rest a = (do
--             f <- op
--             b <- p
--             rest (f a b)) <|> pure a


chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do a <- p
                    rest a
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a


-- successfulParse :: Text -> (Integer, [Maybe Integer])
successfulParse parser input = 
  case parseOnly parser input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right expressions -> expressions
