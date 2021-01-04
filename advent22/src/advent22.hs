-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text hiding (take)
-- import Data.Attoparsec.Combinator
import Control.Applicative
-- import Control.Applicative.Combinators

import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|), (|>))

type Deck = Q.Seq Int
type Game = (Deck, Deck)

data Player = P1 | P2 deriving (Show, Eq)


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent22.txt"
      let decks = successfulParse text
      print decks
      print $ play decks
      print $ part1 decks
      print $ part2 decks

part1 decks = score $ winningDeck $ play decks
part2 decks = score $ snd $ playRecursive decks S.empty

play = until finished playRound 

finished :: Game -> Bool
finished (Empty, _)     = True
finished (_,     Empty) = True
finished (_,     _)     = False

playRound :: Game -> Game
playRound ((x :<| xs), (y :<| ys))
  | x < y     = (xs,           ys |> y |> x)
  | otherwise = (xs |> x |> y, ys)

winningDeck game = case (winner game) of
  P1 -> fst game
  P2 -> snd game

winner :: Game -> Player
winner (Empty, ys) = P2
winner (xs,    _)  = P1

score :: Deck -> Int
score = Q.foldrWithIndex (\i c s -> s + (i + 1) * c) 0 . Q.reverse


playRecursive :: Game -> (S.Set Game) -> (Player, Deck)
playRecursive (Empty, ys) _ = (P2, ys)
playRecursive (xs, Empty) _ = (P1, xs)
playRecursive g@(x :<| xs, y :<| ys) seen 
  | g `S.member` seen = (P1, x :<| xs)
  | (lengthAtLeast x xs) && (lengthAtLeast y ys) = playRecursive subG seen'
  | otherwise = playRecursive compareG seen'
  where seen' = S.insert g seen
        (subWinner, _) = playRecursive (Q.take x xs, Q.take y ys) seen'
        subG = updateDecks subWinner g
        compareWinner = if x < y then P2 else P1
        compareG = updateDecks compareWinner g


updateDecks P1 (x :<| xs, y :<| ys) = (xs |> x |> y, ys)
updateDecks P2 (x :<| xs, y :<| ys) = (xs, ys |> y |> x)

lengthAtLeast n s = Q.length s >= n




-- Parse the input file

decksP = (,) <$> deckP <* (many endOfLine) <*> deckP

headerP = string "Player " *> decimal *> ":" *> endOfLine

deckP = Q.fromList <$> (headerP *> (decimal `sepBy` endOfLine))

successfulParse :: Text -> Game
successfulParse input = 
  case parseOnly decksP input of
    Left  _err -> (Q.empty, Q.empty) -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right decks -> decks
