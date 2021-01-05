-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text hiding (take)
-- import Data.Attoparsec.Combinator
import Control.Applicative
-- import Control.Applicative.Combinators

import qualified Data.Set as S
import qualified Data.IntMap.Strict as M
import qualified Data.Sequence as Q
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|), (|>))

import Data.Foldable (toList)
import Data.Hashable (hash)


type Deck = Q.Seq Int
type Game = (Deck, Deck)

data Player = P1 | P2 deriving (Show, Eq)

type Cache = M.IntMap (S.Set Game)


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent22.txt"
      let decks = successfulParse text
      -- print decks
      -- print $ play decks
      print $ part1 decks
      print $ part2 decks

part1 game = score $ winningDeck $ play game
part2 game = score $ snd $ playRecursive game M.empty

play = until finished playRound 

finished :: Game -> Bool
finished (Empty, _)     = True
finished (_,     Empty) = True
finished (_,     _)     = False

playRound :: Game -> Game
playRound ((x :<| xs), (y :<| ys))
  | x < y     = (xs,           ys |> y |> x)
  | otherwise = (xs |> x |> y, ys)

winningDeck :: Game -> Deck
winningDeck (Empty, ys) = ys
winningDeck (xs,    _)  = xs

score :: Deck -> Int
score = sum . zipWith (*) [1..] . toList . Q.reverse

playRecursive :: Game -> Cache -> (Player, Deck)
playRecursive (Empty, ys) _ = (P2, ys)
playRecursive (xs, Empty) _ = (P1, xs)
playRecursive g@(x :<| xs, y :<| ys) seen 
  | g `inCache` seen = (P1, x :<| xs)
  | (lengthAtLeast x xs) && (lengthAtLeast y ys) = playRecursive subG seen'
  | otherwise = playRecursive compareG seen'
  where seen' = enCache g seen
        (subWinner, _) = playRecursive (Q.take x xs, Q.take y ys) seen'
        subG = updateDecks subWinner g
        compareTops = if x < y then P2 else P1
        compareG = updateDecks compareTops g


updateDecks P1 (x :<| xs, y :<| ys) = (xs |> x |> y, ys)
updateDecks P2 (x :<| xs, y :<| ys) = (xs, ys |> y |> x)

lengthAtLeast n s = Q.length s >= n


hashGame (xs, ys) = 
  hash ( toList $ Q.take 2 xs
       , toList $ Q.take 2 ys
       -- , Q.length xs
       -- , Q.length ys
       )

inCache :: Game -> Cache -> Bool
inCache game cache = case (M.lookup h cache) of
  Just games -> game `S.member` games
  Nothing -> False
  where h = hashGame game

enCache :: Game -> Cache -> Cache
enCache game cache = case (M.lookup h cache) of
  Just games -> M.insert h (S.insert game games) cache
  Nothing -> M.insert h (S.singleton game) cache
  where h = hashGame game


-- Parse the input file

decksP = (,) <$> deckP <* (many endOfLine) <*> deckP

headerP = string "Player " *> decimal *> ":" *> endOfLine

deckP = Q.fromList <$> (headerP *> (decimal `sepBy` endOfLine))

successfulParse :: Text -> Game
successfulParse input = 
  case parseOnly decksP input of
    Left  _err -> (Q.empty, Q.empty) -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right decks -> decks
