-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text hiding (take)
-- import Data.Attoparsec.Combinator
import Control.Applicative
import qualified Data.Map.Strict as M
import Data.List
import Control.Monad.CSP


type RuleSet = M.Map String Body

data Body = Body Range Range -- the two ranges
  deriving (Show, Eq)

data Range = Range Int Int -- lower, upper bounds
  deriving (Show, Eq)

type Ticket = [Int]

type ColCandidateSet = M.Map String [Int]


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent16.txt"
      let (rules, myTicket, nearbyTickets) = successfulParse text
      print $ part1 rules nearbyTickets
      print $ part2 rules myTicket nearbyTickets

part1 = ticketErrorRate

part2 rules myTicket nearbyTickets = product $ M.elems departureTicket
  where 
    columnDomains = possibleColumnsAll rules nearbyTickets
    namedCols = knownCols columnDomains
    filledTicket = buildTicket namedCols myTicket
    departureTicket = M.filterWithKey (\k _ -> "departure" `isPrefixOf` k) filledTicket


inRange (Range lower upper) value = (lower <= value) && (value <= upper)
matchesRule (Body a b) value = (inRange a value) || (inRange b value)

validForAnyField :: RuleSet -> Int -> Bool
validForAnyField rules value = any (flip matchesRule value) $ M.elems rules

ticketErrorRate :: RuleSet -> [Ticket] -> Int
ticketErrorRate rules tickets = 
  sum [ v 
      | t <- tickets
      , v <- t
      , (not $ validForAnyField rules v) ]

isValidTicket :: RuleSet -> Ticket -> Bool
isValidTicket rules ticket = and $ map (validForAnyField rules) ticket

possibleColumnsAll :: RuleSet -> [Ticket] -> ColCandidateSet
possibleColumnsAll rules tickets = M.map (possibleColumns ticketCols) rules
  where validTickets = filter (isValidTicket rules) tickets
        ticketCols = transpose validTickets

possibleColumns ticketCols body = map fst $ filter columnMatches $ zip [0..] ticketCols
  where columnMatches (_, col) = all (matchesRule body) col


knownCols :: ColCandidateSet -> M.Map String Int
knownCols colCandidates = M.fromList $ zip names cols
  where 
    (names, colDomains) = unzip $ M.toList colCandidates
    cols = solveColumns colDomains

solveColumns :: [[Int]] -> [Int]
solveColumns colDomains = oneCSPSolution $ do
  dvs <- mapM mkDV colDomains
  mapAllPairsM_ (constraint2 (/=)) dvs
  return dvs

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ f []     = return ()
mapAllPairsM_ f (_:[]) = return ()
mapAllPairsM_ f (a:l) = mapM_ (f a) l >> mapAllPairsM_ f l

buildTicket :: M.Map String Int -> Ticket -> M.Map String Int
buildTicket namedCols ticketData = M.map (ticketData!!) namedCols


-- Parse the input file

inputP = (,,) <$> rulesP <* blankLines <*> myTicketP <* blankLines <*> nearbyTicketsP

blankLines = skipMany1 endOfLine

rulesP = M.fromList <$> (ruleP `sepBy` endOfLine)

ruleP = (,) <$> nameP <* ": " <*> ruleBodyP
nameP = many1 (letter <|> space)
ruleBodyP = Body <$> rangeP <* " or " <*> rangeP
rangeP = Range <$> decimal <* "-" <*> decimal

myTicketP = "your ticket:" *> endOfLine *> ticketValsP
nearbyTicketsP = "nearby tickets:" *> endOfLine *> (ticketValsP `sepBy` endOfLine)

ticketValsP = decimal `sepBy1` (string ",")

-- successfulParse :: Text -> (Integer, [Maybe Integer])
successfulParse input = 
  case parseOnly inputP input of
    Left  _err -> (M.empty, [], []) -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right ticketInfo -> ticketInfo
